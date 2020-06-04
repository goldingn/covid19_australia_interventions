# use a discrete-time convolution with/without Poisson sampling to forecast from the Reff model
library(dplyr)
library(tidyr)
source("R/functions.R")

linelist <- readRDS("~/not_synced/nnds/linelist_formatted.RDS")

# for now just do single imputation with the mean delay on the date of onset for
# those missing it
delay_samples <- read_csv(
  file = "data/cases/sampled_report_delay.csv",
  col_types = cols(x = col_integer())
)
mean_delay <- round(mean(delay_samples$x))

linelist <- linelist %>%
  mutate(
    date_onset = case_when(
      is.na(date_onset) ~ date_confirmation - mean_delay,
      TRUE ~date_onset
    )
  )

# build date-by-state matrices of the counts of new local and imported cases and
# imports by assumed date of infection (with an incubation period of 5 days)
linelist <- linelist %>%
  rename(state = region,
         date = date_onset) %>%
  mutate(date = date - 5) %>%
  select(-date_confirmation)

import_statuses <- sort(unique(linelist$import_status))
states <- sort(unique(linelist$state))
dates <- seq(min(linelist$date), max(linelist$date), by = 1)

n_states <- length(states)
n_dates <- length(dates)
n_extra <- 21
date_nums <- seq_len(n_dates + n_extra)

# pad this with full set of dates, states, and import statuses
grid <- expand_grid(
  date = dates,
  import_status = import_statuses,
  state = states)

# widen into matrices of date by state
date_by_state <- linelist %>%
  mutate(cases = 1) %>%
  right_join(grid) %>%
  group_by(import_status, state, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = state, values_from = cases) %>%
  select(-date)

imported_cases <- date_by_state %>%
  filter(import_status == "imported") %>%
  select(-import_status) %>%
  as.matrix()

local_cases <- date_by_state %>%
  filter(import_status == "local") %>%
  select(-import_status) %>%
  as.matrix()

scenarios <- c("projection",
               "counterfactual_1",
               "counterfactual_2",
               "counterfactual_3")
for (scenario in scenarios) {
  
  # load Reff posterior summaries
  file <- paste0("outputs/", scenario, "/r_eff_local_estimates.csv")
  df_reff <- read.csv(file, stringsAsFactors = FALSE)
  
  df_reff$date <- as.Date(df_reff$date)
  
  # get mean reff in a date-by-state matrix
  reff_local <- df_reff %>%
    select(date, state, mean) %>%
    pivot_wider(names_from = state, values_from = mean) %>%
    select(-date) %>%
    as.matrix()
  
  reff_imported <- reff_local * 0 + 0.1
  
  n_dates_project <- nrow(reff_local)
  
  # count infections, starting with one in each state
  imported <- local <- infectious <- reff_local * 0
  imported[seq_len(n_dates), ] <- imported_cases
  local[seq_len(n_dates), ] <- local_cases
  
  # contibution of imports to infection of locals on each day
  local_infectious <- apply_serial_interval(local)
  import_infectious <- apply_serial_interval(imported)
  import_contribution <- import_infectious * reff_imported
  
  n_steps <- 20
  # iterate through these, convolving to get the expected numbers of case
  probabilities <- serial_interval_probability(0:n_steps)
  si_disaggregation <- disaggregation_matrix(n_steps, probabilities)
  
  times <- n_dates:(n_dates_project - 1)
  # set.seed(1)
  
  for (t in times) {
    
    from_idx <- seq_len(n_steps) - 1 + t
    to_idx <- from_idx + 1
    
    keep <- to_idx <= n_dates_project
    from_idx <- from_idx[keep]
    to_idx <- to_idx[keep]
    
    # accumulate infectious people
    new_infectious <- si_disaggregation[keep, keep] %*% local[from_idx, ]
    local_infectious[to_idx, ] <- local_infectious[to_idx, ] + new_infectious
    
    local_contribution_tp1 <- local_infectious[t, ] * reff_local[t, ]
    expected_new_infections_tp1 <- local_contribution_tp1 + import_contribution[t + 1, ]
    
    # get newly infected people
    new_infected_tp1 <- rpois(n_states, expected_new_infections_tp1)
    local[t + 1, ] <- new_infected_tp1
    
  }
  
  # plot true and simulated case counts
  
  past <- 21
  future <- n_dates_project - n_dates
  idx_plot <- n_dates + seq_len(past + future) - past
  
  cols <- ifelse(idx_plot > n_dates , "red", grey(0.8))
  
  
  png(paste0("~/Desktop/sim_", scenario, ".png"),
      width = 1100, height = 1300,
      pointsize = 30)
  par(mfrow =  c(4, 2),
      las = 1,
      mar = c(1, 2, 1, 1),
      oma = c(2, 2, 2, 0))
  for (i in 1:n_states) {
    
    bp <- barplot(local[idx_plot, i], col = cols, border = NA,
                  xlab = "",
                  ylab = "new cases",
                  ylim = c(0, 10))
    
    mtext(text = colnames(reff_local)[i],
          side = 3, adj = 0.05, cex = 0.8, line = -1)
    
    # vertical lines for May 11th and 4 weeks later
    change <- which(idx_plot == which(dates == (as.Date("2020-05-11") - 10)))
    abline(v = bp[change], lty = 2)
    abline(v = bp[change + 28], lty = 2)
    
    idx <- seq(1, length(bp), by = 6)
    
    dates_start <- min(dates + min(idx_plot) + 10)
    dates_plot <- format(dates_start + bp - bp[1], "%b %d")
    labels <- dates_plot[idx]
    if (!(i %in% 7:8)) {
      labels = NA
    }
    axis(1, labels = labels, at = bp[idx])
  }
  mtext("new cases", side = 2, outer = TRUE, las = 0, line = 0.6)
  mtext(paste("Simulation of new cases under", scenario), side = 3, outer = TRUE, adj = 0)
  dev.off()
  
}

# project Reff draws with wiggles (need to fit parameters)
# simplify GP on import Reff for a speedup?
