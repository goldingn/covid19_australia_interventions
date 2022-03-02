# microdistancing comparison

micro_plotting_data <- readRDS("outputs/micro_plotting_data.RDS")

line_df <- micro_plotting_data$line_df %>%
  mutate(
    year = if_else(date < "2021-07-01", "20/21", "21/22"),
    date = if_else(date >= "2021-07-01", date - years(1), date)
  ) %>%
  filter(
    date <= "2021-02-16",
    date >= "2020-11-01"
  )
point_df <- micro_plotting_data$point_df  %>%
  mutate(
    year = if_else(date < "2021-07-01", "20/21", "21/22"),
    date = if_else(date >= "2021-07-01", date - years(1), date)
  ) %>%
  filter(
    date <= "2021-02-16",
    date >= "2020-11-01"
  )


line_df 
point_df

col1 <- "grey"
col2 <- "darkorchid1"

p <- ggplot(line_df) +
  
  aes(date, mean, fill = year, colour = year) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "2 week", date_labels = "%e/%m") +
  scale_alpha(range = c(0, 0.5)) +
  #scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions(),
    colour = "grey80"
  ) +
  
  geom_ribbon(aes(ymin = ci_90_lo,
                  ymax = ci_90_hi,),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = ci_50_lo,
                  ymax = ci_50_hi),
              alpha = 0.5) +
  geom_line(aes(y = ci_90_lo),
            alpha = 0.8) + 
  geom_line(aes(y = ci_90_hi),
            alpha = 0.8) + 
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 7)) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage, colour = year),
    data = point_df,
    size = 3,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper, colour = year),
    data = point_df,
    size = 2,
    alpha = 0.4,
    width = 0
  ) +
  scale_colour_manual(values = c(col1, col2)) +
  scale_fill_manual(values = c(col1, col2)) +
  
  # and titles  
  ggtitle(
    label = "Micro-distancing trend",
    subtitle = "Calibrated against self-reported adherence to physical distancing"
  ) +
  labs( y = "Estimate of percentage 'always' keeping 1.5m distance", colour = "Year", fill = "Year")

p


save_ggplot("microdistancing_effect_year_comparison.png")


############################
# macrodistancing comparison

# informative priors for baseline contact parameters
baseline_contact_params <- baseline_contact_parameters(gi_cdf)

# data for plotting
baseline_point <- tibble::tibble(
  date = as.Date("2020-03-01"),
  estimate = baseline_contact_params$mean_contacts[2],
  sd = baseline_contact_params$se_contacts[2],
  type = "Nowcast"
) %>%
  mutate(
    lower = estimate - sd * 1.96,
    upper = estimate + sd * 1.96
  )

data <- macrodistancing_data()
params <- macrodistancing_params(baseline_contact_params)
predictions <- macrodistancing_model(data, params)
out <- macrodistancing_likelihood(predictions, data)


fitted_model <- readRDS("outputs/fitted_macro_model.RDS")

# make predictions using updated data on a day out of sync with standard Monday fit
fitted_model$data <- data
fitted_model$predictions <- macrodistancing_model(fitted_model$data, fitted_model$params)


nsim <- coda::niter(fitted_model$draws) * coda::nchain(fitted_model$draws)
nsim <- min(10000, nsim)

# check posterior calibration
sdlog <- fitted_model$out$sdlog
meanlog <- log(fitted_model$out$predictions) - (sdlog ^ 2) / 2
contacts_ga <- discrete_lognormal(
  meanlog = meanlog,
  sdlog = sdlog,
  breaks = fitted_model$data$breaks
)
# contacts_sim <- calculate(contacts_ga, values = fitted_model$draws, nsim = nsim)[[1]][, , 1]
# bayesplot::ppc_ecdf_overlay(
#   fitted_model$data$contacts$contact_num,
#   contacts_sim[1:1000, ],
#   discrete = TRUE
# ) +
#   coord_cartesian(xlim = c(0, 300))
## THIS HASHED OUT AS KILLING GR LAPTOP MEMORY

OC_t_state <- fitted_model$predictions$mean_daily_contacts

# get trend predictions
pred_sim <- calculate(c(OC_t_state), values = fitted_model$draws, nsim = nsim)[[1]][, , 1]
quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

# predicted trends for downstream modelling
pred_trend <- fitted_model$data$location_change_trends %>%
  select(date, state) %>%
  # add predictions
  mutate(mean = colMeans(pred_sim)) %>%
  bind_cols(as_tibble(quants))

plot_data <- list(
  dates = list(
    infection_project = dates,
    latest_mobility = max(dates)
  ),
  states = states,
  n_states = length(states),
  n_dates_project = length(dates)
)

# non-household contacts
p <- plot_trend(pred_sim,
                data = plot_data,
                multistate = TRUE,
                base_colour = purple,
                max_date = max(data$contacts$date),
                ylim = c(0, 20),
                hline_at = NULL) + 
  ggtitle(label = "Macro-distancing trend",
          subtitle = "Rate of non-household contacts") +
  ylab("Estimated mean number of non-household contacts per day") + 
  
  # add baseline estimate
  geom_point(
    aes(date, estimate),
    data = baseline_point,
    size = 0.5,
    colour = grey(0.5)
  ) +
  geom_errorbar(
    aes(
      date,
      estimate,
      ymin = lower,
      ymax = upper
    ),
    data = baseline_point,
    width = 0,
    colour = grey(0.5)
  ) + 
  
  # rug marks for holidays
  geom_rug(
    aes(date),
    data = holiday_lines,
    col = green,
    size = 1,
    length = unit(0.05, "npc"),
    sides = "b",
    inherit.aes = FALSE
  ) +
  
  # add survey results estimate
  geom_point(
    aes(
      wave_date,
      estimate,
    ),
    data = survey_points,
    size = 2,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(
      wave_date,
      estimate,
      ymin = lower,
      ymax = upper,
    ),
    data = survey_points,
    size = 1,
    alpha = 0.2,
    width = 0
  )

p

save_ggplot("macrodistancing_effect.png")



