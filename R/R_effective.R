# fit a Bayesian model-based estimate of R_effective over time, quantifying the
# impacts of both quarantine and physical distancing measures.

source("R/lib.R")

set.seed(2020-04-29)
source("R/functions.R")

# sync up the case data
sync_nndss()

# prepare data for Reff modelling
linelist <- readRDS("outputs/commonwealth_ll_imputed_old_method.RDS")
old_delay_cdf <- readRDS("outputs/old_method_delay_cdf.RDS")
data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = old_delay_cdf)
#reload data here to get the latest vaccine effect, which is typically computed after linelist

#data <- readRDS("outputs/pre_loaded_reff_data_old_imputation.RDS")
# #quick check if reff data is already loaded
# if (length(data) != 12) {
#   data <- reff_model_data() 
#   saveRDS(data, "outputs/pre_loaded_reff_data.RDS")
# }
data$dates$linelist

# save the key dates for Freya and David to read in, and tabulated local cases
# data for the Robs

write_reff_key_dates(data)
write_local_cases(data)

# format and write out any new linelists to the past_cases folder for Rob H
#update_past_cases()

# define the model (and greta arrays) for Reff, and sample until convergence
fitted_model <- fit_reff_model(data)

# save the fitted model object
saveRDS(fitted_model, "outputs/fitted_reff_model.RDS")
# fitted_model <- readRDS("outputs/fitted_reff_model.RDS")


# visual checks of model fit
plot_reff_checks(fitted_model)


# output Reff trajectory draws for Rob M
write_reff_sims(fitted_model, dir = "outputs/projection")


vaccine_effect_timeseries <- readRDS(file = "outputs/vaccination_effect.RDS")

# write sims of C1 without vaccine effect
write_reff_sims_novax(
  fitted_model#,
  #vaccine_timeseries = vaccine_effect_timeseries
)

# generatge sims for plotting
# (saves repeat generation of sims in each reff_plotting call and keeps them consistent)
sims <- reff_plotting_sims(fitted_model)

# do plots for main period
reff_plotting(
  fitted_model,
  dir = "outputs",
  sims = sims
)

# most recent six months
reff_plotting(
  fitted_model,
  dir = "outputs",
  subdir = "figures/six_month",
  min_date = NA,
  sims = sims
)

# most recent month
reff_plotting(
  fitted_model,
  dir = "outputs",
  subdir = "figures/one_month",
  min_date = fitted_model$data$dates$latest_mobility - months(1),
  sims = sims
)


# most recent month no nowcast
reff_plotting(
  fitted_model,
  dir = "outputs",
  subdir = "figures/one_month/no_nowcast",
  min_date = fitted_model$data$dates$latest_mobility - months(1),
  max_date = fitted_model$data$dates$latest_infection,
  sims = sims,
  mobility_extrapolation_rectangle = FALSE
)


# most recent six months no nowcast
reff_plotting(
  fitted_model,
  dir = "outputs",
  subdir = "figures/six_month/no_nowcast",
  min_date = NA,
  max_date = fitted_model$data$dates$latest_infection,
  sims = sims,
  mobility_extrapolation_rectangle = FALSE
)

# projection plots 
reff_plotting(
  fitted_model,  
  dir = "outputs/projection",
  max_date = fitted_model$data$dates$latest_project,
  mobility_extrapolation_rectangle = FALSE,
  projection_date = fitted_model$data$dates$latest_mobility,
  sims = sims
)

# 6-month projection plots
reff_plotting(
  fitted_model,
  dir = "outputs/projection",
  subdir = "figures/six_month",
  min_date = NA,
  max_date = fitted_model$data$dates$latest_project,
  mobility_extrapolation_rectangle = FALSE,
  projection_date = fitted_model$data$dates$latest_mobility,
  sims = sims
)


# produce simulations where proportion of variant is constant
simulate_variant(variant = "wt")
simulate_variant(variant = "alpha")
simulate_variant(variant = "delta")
simulate_variant(variant = "omicron")

simulate_variant(variant = "alpha", subdir = "alpha/ratio", ratio_samples = TRUE)
simulate_variant(variant = "delta", subdir = "delta/ratio", ratio_samples = TRUE)
simulate_variant(variant = "omicron", subdir = "omicron/ratio", ratio_samples = TRUE)



#simulate variant with vax effect

simulate_variant(
  variant = "omicron",
  subdir = "omicron_vax",
  vax_effect = vaccine_effect_timeseries %>% 
    filter(variant == "Omicron", 
           date <= max(fitted_model$data$dates$infection_project)) %>% 
    select(-variant,-percent_reduction)
)


simulate_variant(
  variant = "delta",
  subdir = "delta_vax",
  vax_effect = vaccine_effect_timeseries %>% 
    filter(variant == "Delta", 
           date <= max(fitted_model$data$dates$infection_project)) %>% 
    select(-variant,-percent_reduction)
)





#simulate variant with combined immunity effect

combined_effect_timeseries_full <- readRDS("outputs/combined_effect_full.RDS")

simulate_variant(
  variant = "omicron",
  subdir = "omicron_combined/",
  vax_effect = combined_effect_timeseries_full %>% 
    filter(
      variant == "Omicron", 
      date <= max(fitted_model$data$dates$infection_project),
      ascertainment == 0.5
    ) %>% 
    select(-variant,-percent_reduction, -ascertainment)
)


simulate_variant(
  variant = "delta",
  subdir = "delta_combined",
  vax_effect = combined_effect_timeseries_full %>% 
    filter(
      variant == "Delta", 
      date <= max(fitted_model$data$dates$infection_project),
      ascertainment == 0.5
    ) %>% 
    select(-variant,-percent_reduction, -ascertainment)
)


source("R/omicron_delta_combined_compare.R")

#   plot the new 3-way TP comparison (no vax vs vax vs hybrid)
no_infection_immunity_c1 <- read_csv(paste0("outputs/projection/r_eff_1_local_samples.csv"),
                                     col_types =cols(
                                       .default = col_double(),
                                       date = col_date(format = ""),
                                       state = col_character(),
                                       date_onset = col_date(format = "")
                                     )) 

no_vax_or_infection_immunity_c1 <- read_csv(paste0("outputs/projection/r_eff_1_local_without_vaccine_samples.csv"),
                                            col_types =cols(
                                              .default = col_double(),
                                              date = col_date(format = ""),
                                              state = col_character(),
                                              date_onset = col_date(format = "")
                                            )) 


#plot 
start.date <- ymd("2021-02-01")
end.date <- the.date
date.label.format <- "%b %y"
n.week.labels.panel <- 2
n.week.ticks <- "1 month"

# Create date objects for ticks/labels (e.g., show ticks every n.week.ticks, but label every n.week.labels.panel)
dd <- format(seq.Date(ymd(start.date), end.date, by=n.week.ticks), date.label.format)
dd.labs <- as.character(dd)
dd.labs[!(dd.labs %in% dd.labs[(seq(length(dd.labs),1,by=-n.week.labels.panel))])] <- ""
dd.labs <- gsub(pattern = "^0", replacement = "", x = dd.labs)
dd.labs <- gsub(pattern = "/0", replacement = "/", x = dd.labs)
dd <- seq.Date(ymd(start.date), end.date, by=n.week.ticks)

# Quantiles
qs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

r1 <- no_vax_or_infection_immunity_c1 %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95") %>% filter(date <= end.date)


r2 <- no_infection_immunity_c1 %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95")  %>% filter(date <= end.date)

r2.post <- r2 %>% filter(date >= vacc.start)



r3 <- omicron_combined %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95")  %>% filter(date <= end.date)

r3.post <- r3 %>% filter(date >= "2021-12-01")


# Plot aesthetics
outer.alpha <- 0.15
inner.alpha <- 0.4
line.alpha <- 0.8
col1 <- "grey70"
col2 <- "grey50"
col3 <- green


subset.states <- c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA")

ggplot() +
  ggtitle(
    label = "Transmission potential",
    subtitle = "Wth and without the effects of vaccination, and the effect of immunity from infection with Omicron variant"
  ) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions() %>% filter(state %in% subset.states),
    colour = "grey80"
  ) +
  
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col1, alpha=outer.alpha) +
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col1, alpha=inner.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col1, alpha = line.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col1, alpha = line.alpha) +
  
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col2, alpha=outer.alpha) +
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col2, alpha=inner.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col2, alpha = line.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col2, alpha = line.alpha) +
  
  geom_ribbon(data = r3.post %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col3, alpha=outer.alpha) +
  geom_ribbon(data = r3.post %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col3, alpha=inner.alpha) +
  geom_line(data = r3.post %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col3, alpha = line.alpha) +
  geom_line(data = r3.post %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col3, alpha = line.alpha) +
  # geom_vline(
  #   data = prop_variant_dates() %>% filter(state %in% subset.states),
  #   aes(xintercept = date),
  #   colour = "firebrick1",
  #   linetype = 5
  # ) +
  
  geom_vline(xintercept = vacc.start, colour = "steelblue3", linetype = 5) +
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  scale_y_continuous("", position = "right", breaks = seq(0,5,by=1)) +
  scale_x_date("Date", breaks = dd, labels = dd.labs) +
  # scale_x_date("Date", date_breaks = "1 month", date_labels = "%e/%m") +
  
  coord_cartesian(xlim = c(start.date, end.date),
                  ylim = c(0, 5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(size = 9),
        panel.spacing = unit(1.2, "lines")) + 
  geom_vline(
    data = prop_variant_dates(),
    aes(xintercept = date),
    colour = "firebrick1",
    linetype = 5
  )

ggsave(paste0("outputs/figures/full_tp_compare_",the.date,".png"), height = 10, width = 9, bg = "white")
