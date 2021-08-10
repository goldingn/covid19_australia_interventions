source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)
source("./objects_and_settings.R")

fitted_model <- readRDS("outputs/fitted_reff_model.RDS")

reff_plotting <- function(
  fitted_model,
  dir = "outputs",
  min_date = as.Date("2020-03-01"),
  #min_date = NA,
  max_date = fitted_model$data$dates$latest_mobility,
  mobility_extrapolation_rectangle = TRUE,
  projection_date = NA,
  washout_cutoff = 0,
  vaccine_timeseries = timeseries
) {
  
  if(is.na(min_date)) {
    min_date <- max_date - months(6)
  }
  
  # reformat case data for plotting (C1 and C12)
  local_cases_long <- fitted_model$data$local$cases %>%
    as_tibble() %>%
    mutate(
      date = fitted_model$data$dates$infection,
    ) %>%
  pivot_longer(
    cols = -date,
    names_to = "state",
    values_to = "cases"
  )
  
  # rugplot of case counts
  case_data <- local_cases_long %>%
    filter(date >= min_date) %>%
    mutate(
      type = "Nowcast",
      height = 1
    ) %>%
    uncount(cases)
  
  case_rug <- geom_rug(
    aes(date, height),
    data = case_data,
    sides = "b",
    alpha = 0.5,
    size = 0.5,
    colour = grey(0.7)
  )
  
  # a washout for whether there are few cases (<20 in past 5 days) with a run of
  # at least 7 days (to prevent rapidly flip-flopping)
  few_case_data <- local_cases_long %>%
    full_join(
      expand_grid(
        state = fitted_model$data$states,
        date = seq(min_date, max_date, by = 1),
      )
    ) %>%
    mutate(
      cases = replace_na(cases, 0)
    ) %>%
    group_by(state)  %>%
    mutate(
      recent_count = slide_int(cases, sum, .before = 13),
    ) %>%
    ungroup() %>%
    filter(date >= min_date) %>%
    mutate(
      few_cases = recent_count < washout_cutoff,
      type = "Nowcast",
      state = factor(state),
      mean = 1
    ) %>%
    # don't let the washout state change until there have been at least the
    # specified number of days in the same state
    arrange(state, date) %>%
    group_by(state) %>%
    mutate(
      washout = constrain_run_length(few_cases, 7)
    ) %>%
    ungroup()
  
  few_case_washout <- geom_ribbon(
    aes(ymin = -10, ymax = washout * 100 - 10),
    data = few_case_data,
    fill = grey(1),
    alpha = 0.5,
    colour = grey(0.9),
    linetype = 3
  )
  
  
  
  # add counterfactuals to the model object:
  # add fitted_model_extended obect because fitted_model is modified
  fitted_model_extended <- fitted_model
  # Reff for locals component 1 under
  # only micro/macro/surveillance improvements
  fitted_model_extended$greta_arrays <- c(
    fitted_model$greta_arrays,
    list(
      R_eff_loc_1_macro = reff_1_only_macro(fitted_model_extended),
      R_eff_loc_1_micro = reff_1_only_micro(fitted_model_extended),
      R_eff_loc_1_surv = reff_1_only_surveillance(fitted_model_extended),
      R_eff_loc_1_vaccine_effect = reff_1_vaccine_effect(fitted_model_extended, vaccine_timeseries)
    ) 
  )
  
  # flatten all relevant greta array matrices to vectors before calculating
  trajectory_types <- c(
    "R_eff_loc_1",
    "R_eff_imp_1",
    "R_eff_loc_12",
    "R_eff_imp_12",
    "epsilon_L",
    "R_eff_loc_1_micro",
    "R_eff_loc_1_macro",
    "R_eff_loc_1_surv",
    "R_eff_loc_1_vaccine_effect"
  )
  vector_list <- lapply(fitted_model_extended$greta_arrays[trajectory_types], c)
  
  # simulate from posterior for these quantities of interest
  args <- c(vector_list, list(values = fitted_model_extended$draws, nsim = 10000))
  sims <- do.call(calculate, args)
  
  # vaccine effect only
  plot_trend(sims$R_eff_loc_1_vaccine_effect[,1:fitted_model_extended$data$n_date_nums,], # clunky fix
             data = fitted_model_extended$data,
             min_date = min_date,
             max_date = max_date,
             multistate = FALSE,
             base_colour = fifo,
             projection_at = projection_date,
             ylim = c(0, 5),
             intervention_at = vaccination_dates(),
             plot_voc = TRUE
             ) + 
    ggtitle(label = "Impact of vaccination",
            subtitle = expression(R["eff"]~"if"~only~vaccination~had~occurred)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_vaccine_effect.png", dir, multi = FALSE)
  
  
  # microdistancing only
  plot_trend(sims$R_eff_loc_1_micro,
             data = fitted_model_extended$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = purple,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of micro-distancing",
            subtitle = expression(R["eff"]~"if"~only~"micro-distancing"~behaviour~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_micro.png", dir)
  
  # macrodistancing only
  plot_trend(sims$R_eff_loc_1_macro,
             data = fitted_model_extended$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = blue,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of macro-distancing",
            subtitle = expression(R["eff"]~"if"~only~"macro-distancing"~behaviour~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_macro.png", dir)
  
  # improved surveilance only
  plot_trend(sims$R_eff_loc_1_surv,
             data = fitted_model_extended$data,
             max_date = max_date,
             multistate = TRUE,
             base_colour = yellow,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of improved surveillance",
            subtitle = expression(R["eff"]~"if"~only~surveillance~effectiveness~had~changed)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local_surv.png", dir)
  
  # Component 1 for national / state populations
  plot_trend(sims$R_eff_loc_1,
             data = fitted_model_extended$data,
             min_date = min_date,
             max_date = max_date,
             multistate = TRUE,
             base_colour = green,
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of social distancing",
            subtitle = expression(Component~of~R["eff"]~due~to~social~distancing)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_local.png", dir)
  
  plot_trend(sims$R_eff_imp_1,
             data = fitted_model_extended$data,
             min_date = min_date,
             max_date = max_date,
             multistate = FALSE,
             base_colour = orange,
             ylim = c(0, 0.4),
             intervention_at = quarantine_dates(),
             projection_at = projection_date,
             plot_voc = TRUE) + 
    ggtitle(label = "Impact of quarantine of overseas arrivals",
            subtitle = expression(Component~of~R["eff"]~due~to~quarantine~of~overseas~arrivals)) +
    ylab(expression(R["eff"]~component))
  
  save_ggplot("R_eff_1_import.png", dir, multi = FALSE)
  
  # Reff for active cases
  p <- plot_trend(sims$R_eff_loc_12,
                  data = fitted_model_extended$data,
                  min_date = min_date,
                  max_date = max_date,
                  multistate = TRUE,
                  base_colour = green,
                  ylim = c(0, 5),
                  projection_at = projection_date,
                  plot_voc = TRUE) +
    ggtitle(label = "Local to local transmission potential",
            subtitle = "Average across active cases") +
    ylab(expression(R["eff"]~from~"locally-acquired"~cases))
  
  if (mobility_extrapolation_rectangle) {
    p <- p + annotate("rect",
                      xmin = fitted_model_extended$data$dates$latest_infection,
                      xmax = fitted_model_extended$data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  # add case rug plot and washout
  p <- p + few_case_washout + case_rug
  p
  
  save_ggplot("R_eff_12_local.png", dir)
  
  # component 2 (noisy error trends)
  p <- plot_trend(sims$epsilon_L,
                  data = fitted_model_extended$data,
                  min_date = min_date,
                  max_date = max_date,
                  multistate = TRUE,
                  base_colour = pink,
                  hline_at = 0,
                  projection_at = projection_date,
                  ylim = NULL,
                  plot_voc = TRUE) + 
    ggtitle(label = "Short-term variation in local to local transmission rates",
            subtitle = expression(Deviation~from~log(R["eff"])~of~"local-local"~transmission)) +
    ylab("Deviation")
  
  if (mobility_extrapolation_rectangle) {
    p <- p + annotate("rect",
                      xmin = fitted_model_extended$data$dates$latest_infection,
                      xmax = fitted_model_extended$data$dates$latest_mobility,
                      ymin = -Inf,
                      ymax = Inf,
                      fill = grey(0.5), alpha = 0.1)
    
  }
  
  # add case rug plot and washout
  p <- p + case_rug + few_case_washout
  
  p
  
  save_ggplot("R_eff_2_local.png", dir)
  
}


plot_trend_long(
  simulations = sims$R_eff_loc_1_micro,
  data = fitted_model_extended$data,
  min_date = min_date,
  max_date = max_date,
  multistate = TRUE,
  base_colour = purple,
  projection_at = projection_date,
  plot_voc = TRUE
) + 
  ggtitle(label = "Impact of micro-distancing",
          subtitle = expression(R["eff"]~"if"~only~"micro-distancing"~behaviour~had~changed)) +
  ylab(expression(R["eff"]~component))





plot_trend_long <- function(
  simulations,
  data,
  base_colour = grey(0.4),
  multistate = FALSE,
  hline_at = 1,
  ylim = c(0, 5),
  intervention_at = interventions(),
  projection_at = NA,
  keep_only_rows = NULL,
  max_date = data$dates$latest_mobility,
  min_date = as.Date("2020-03-01"),
  plot_voc = FALSE
) {
  
  mean <- colMeans(simulations)
  ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95))
  ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))
  
  if (multistate) {
    states <- rep(data$states, each = data$n_dates_project)
    dates <- rep(data$dates$infection_project, data$n_states)
  } else {
    dates <- data$dates$infection_project
    states <- NA
  }
  
  df <- tibble(date = dates,
               state = states,
               mean = mean,
               ci_50_lo = ci_50[1, ],
               ci_50_hi = ci_50[2, ],
               ci_90_lo = ci_90[1, ],
               ci_90_hi = ci_90[2, ])
  
  if (!is.null(keep_only_rows)) {
    df <- df[keep_only_rows, ]
  }
  
  df <- df %>%
    filter(
      date >= min_date,
      date <= max_date
    ) %>%
    mutate(type = "Nowcast")
  
  if (is.null(ylim)) {
    ylim <- c(min(df$ci_90_lo), max(df$ci_90_hi)) 
  }
  
  p <- ggplot(df) + 
    
    aes(date, mean, fill = type) +
    
    xlab(element_blank()) +
    
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = "1 month", date_labels = "%e/%m") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey80"
    ) +
    
    geom_ribbon(aes(ymin = ci_90_lo,
                    ymax = ci_90_hi),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_50_lo,
                    ymax = ci_50_hi),
                alpha = 0.5) +
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) + 
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) + 
    
    geom_hline(yintercept = hline_at, linetype = "dotted") +
    
    theme_cowplot() +
    panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          #panel.spacing = unit(0.9, "lines"),
          axis.text.x = element_text(size = 8))
  
  if(plot_voc){
    p <- p + 
      geom_vline(
        data = prop_voc_date_state(),
        aes(xintercept = date),
        colour = "firebrick1",
        linetype = 5
      ) # this may caus problems if plot_voc is TRUE but multistate is FALSE
  }
  
  if (multistate) {
    p <- p + facet_wrap(
      facets = ~ state,
      ncol = 1,
      scales = "free",
      strip.position = "left"
    ) +
      theme(
        strip.text.y.left = element_text(
          hjust = 0,
          vjust = 1,
          face = "bold",
          angle = 0
        )
      )
  }
  
  if (!is.na(projection_at)) {
    p <- p +
      geom_vline(xintercept = projection_at, linetype = "dashed", colour = "grey60") +
      annotate("rect",
               xmin = projection_at,
               xmax = max(df$date),
               ymin = -Inf,
               ymax = Inf,
               fill = grey(0.5), alpha = 0.1)
  }
  
  p    
  
}


plot_trend <- function(
  simulations,
  data,
  base_colour = grey(0.4),
  multistate = FALSE,
  hline_at = 1,
  ylim = c(0, 5),
  intervention_at = interventions(),
  projection_at = NA,
  keep_only_rows = NULL,
  max_date = data$dates$latest_mobility,
  min_date = as.Date("2020-03-01"),
  plot_voc = FALSE
) {
  
  
  mean <- colMeans(simulations)
  ci_90 <- apply(simulations, 2, quantile, c(0.05, 0.95))
  ci_50 <- apply(simulations, 2, quantile, c(0.25, 0.75))
  
  if (multistate) {
    states <- rep(data$states, each = data$n_dates_project)
    dates <- rep(data$dates$infection_project, data$n_states)
  } else {
    dates <- data$dates$infection_project
    states <- NA
  }
  
  df <- tibble(date = dates,
               state = states,
               mean = mean,
               ci_50_lo = ci_50[1, ],
               ci_50_hi = ci_50[2, ],
               ci_90_lo = ci_90[1, ],
               ci_90_hi = ci_90[2, ])
  
  if (!is.null(keep_only_rows)) {
    df <- df[keep_only_rows, ]
  }
  
  df <- df %>%
    filter(
      date >= min_date,
      date <= max_date
    ) %>%
    mutate(type = "Nowcast")
  
  if (is.null(ylim)) {
    ylim <- c(min(df$ci_90_lo), max(df$ci_90_hi)) 
  }
  
  p <- ggplot(df) + 
    
    aes(date, mean, fill = type) +
    
    xlab(element_blank()) +
    
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(position = "right") +
    scale_x_date(date_breaks = "1 month", date_labels = "%e/%m") +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +
    
    
    geom_vline(
      aes(xintercept = date),
      data = intervention_at,
      colour = "grey75"
    ) +
    
    geom_ribbon(aes(ymin = ci_90_lo,
                    ymax = ci_90_hi),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_50_lo,
                    ymax = ci_50_hi),
                alpha = 0.5) +
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) + 
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) + 
    
    geom_hline(yintercept = hline_at, linetype = "dotted") +
    
    theme_cowplot() +
    panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 7))
  
  if(plot_voc){
    p <- p + 
      geom_vline(
        data = prop_voc_date_state(),
        aes(xintercept = date),
        colour = "firebrick1",
        linetype = 5
      ) # this may caus problems if plot_voc is TRUE but multistate is FALSE
  }
  
  if (multistate) {
    p <- p + facet_wrap(~ state, ncol = 2, scales = "free")
  }
  
  if (!is.na(projection_at)) {
    p <- p +
      geom_vline(xintercept = projection_at, linetype = "dashed", colour = "grey60") +
      annotate("rect",
               xmin = projection_at,
               xmax = max(df$date),
               ymin = -Inf,
               ymax = Inf,
               fill = grey(0.5), alpha = 0.1)
  }
  
  p    
  
}
