source("R/lib.R")

source("R/functions.R")

nindss <- load_linelist(use_vic = FALSE,
                        use_nsw = FALSE)

notification_delay_cdf <- get_notification_delay_cdf(nindss)

week_grid <- nindss %>%
  filter(import_status == "local") %>%
  expand_grid(
  state = unique(.$state),
  month = .$date_confirmation %>%
    format("%Y-%m") %>%
    unique
)

missingness <- nindss %>%
  filter(import_status == "local") %>%
  mutate(
    month = format(date_confirmation, "%Y-%m")
  ) %>%
  group_by(
    state,
    month
  ) %>%
  summarise(
    n_cases = n(),
    prop_onset = sum(!is.na(date_onset))/n(),
    prop_quarantine = sum(!is.na(date_quarantine))/n(),
    .groups = "drop"
  ) %>%
  full_join(
    month_grid
  ) %>%
   mutate(
     n_cases = ifelse(is.na(n_cases), 0, n_cases)
   ) %>%
   arrange(month, state)
  

missingness %>%
  ggplot() +
  geom_point(
    aes(
      x = month,
      y = prop_onset,
      size = log10(n_cases)
    ),
    col = "springgreen2"
  ) +
  facet_wrap(
    ~ state,
    ncol = 2
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Proportion",
    size = expression(Log["10"]~"(cases)")
  ) +
  ggtitle(
    label = "Proportion symptom onset dates",
    subtitle = "Proportion of local cases in NINDSS with symptom onset dates recorded, by month"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90),
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(size = 9, angle = 270)
  ) +
  scale_y_continuous(
    position = "right",
    limits = c(-0.2, 1.2),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
  ) 

save_ggplot("proportion_with_onset_date.png", multi = TRUE)

# 
# missingness %>%
#   ggplot() +
#   geom_point(
#     aes(
#       x = month,
#       y = prop_quarantine,
#       size = log10(n_cases)
#     ),
#     col = "gold"
#   ) +
#   facet_wrap(
#     ~ state,
#     ncol = 2
#   ) +
#   theme_classic() +
#   labs(
#     x = NULL,
#     y = "Proportion",
#     size = expression(Log["10"]~"(cases)")
#   ) +
#   ggtitle(
#     label = "Proportion date entered quarantine",
#     subtitle = "Proportion of local cases in NINDSS with date entered quarantine, by month"
#   ) +
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#   theme(
#     legend.position = "bottom",
#     strip.background = element_blank(),
#     strip.text = element_text(hjust = 0, face = "bold"),
#     axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#     panel.spacing = unit(1, "lines"),
#     axis.text.x = element_text(size = 9, angle = 270)
#   ) +
#   scale_y_continuous(
#     position = "right",
#     limits = c(-0.2, 1.2),
#     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
#   ) 
# 
# save_ggplot("proportion_with_quarantine_date.png", multi = TRUE)

# # impute onset dates and infection dates using this
# nindss <- nindss %>%
#   impute_linelist(notification_delay_cdf = notification_delay_cdf)
# 
# nindss <- nindss %>%
#   mutate(
#     date_quarantine = as_date(ifelse(is.na(date_quarantine), date_confirmation, date_quarantine))
#   )
# 
# 
# linelist_date <- nindss$date_linelist[1]
# 
# isolation_data <- nindss %>%
#   # impute onset dates here
#   filter(
#     import_status == "local",
#     !is.na(date_onset),
#     !is.na(date_quarantine),
#   ) %>%
#   mutate(
#     date_infection = date_onset - 5,
#     time_to_isolation = as.numeric(date_quarantine - date_onset),
#     time_to_detection = as.numeric(date_detection - date_onset), # date detection vs date confirmation in rolling delays?
#     .after = date_confirmation
#   ) %>%
#   filter(
#     time_to_isolation < 50,
#     time_to_isolation > -50
#   ) %>%
#   filter(
#     date_onset <= (linelist_date - 15)
#   )
# 
# isolation_data %>%
#   mutate(
#     ym = format(date_confirmation, "%Y-%m")
#   ) %>%
#   ggplot() +
#   geom_boxplot(
#     aes(
#       ym,
#       time_to_isolation
#     )
#   ) +
#   facet_wrap(
#     ~ state,
#     ncol = 2
#   ) +
#   theme_classic() +
#   labs(
#     x = NULL,
#     y = "Time to isolation",
#     col = "State"
#   ) +
#   ggtitle(
#     label = "Time to isolation",
#     subtitle = "Time to isolation by month"
#   ) +
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#   theme(
#     legend.position = "right",
#     strip.background = element_blank(),
#     strip.text = element_text(hjust = 0, face = "bold"),
#     axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#     panel.spacing = unit(1.2, "lines"),
#     axis.text.x = element_text(size = 9, angle = 270)
#   ) +
#   scale_y_continuous(
#     position = "right"
#   ) 
# 
# 
# save_ggplot("time_to_isolation_boxes.png", multi = TRUE)
# 
# isolation_delays_from_onset <- isolation_data %$%
#   estimate_delays(
#     state = state,
#     date = date_onset,
#     delay = time_to_isolation,
#     direction = "forward",
#     national_exclusions = tibble(
#       state = "VIC",
#       start = as.Date("2020-06-14"),
#       end = as.Date("2020-12-01")
#     )
#   )
# 
# # detection_delays_from_onset <- isolation_data %$%
# #   estimate_delays(
# #     state = state,
# #     date = date_onset,
# #     delay = time_to_detection,
# #     direction = "forward"
# #   )
# 
# p_isolation <- isolation_delays_from_onset %>%
#   plot_delays(
#     date = isolation_data$date_onset,
#     state = isolation_data$state,
#     delay = isolation_data$time_to_isolation,
#     ylim = c(-20, 20),
#     base_colour = "purple"
#   ) +
#   ggtitle(
#     label = "Isolation trend",
#     subtitle = "Time from modelled symptom onset to isolation for locally-acquired cases"
#   ) +
#   xlab("Symptom onset date") +
#   ylab("Days to case isolation")
# 
# p_isolation
# 
# save_ggplot("time_to_isolation.png", multi = TRUE)
# 
# 
# 
# # p_detection <- detection_delays_from_onset %>%
# #   plot_delays(
# #     date = isolation_data$date_onset,
# #     state = isolation_data$state,
# #     delay = isolation_data$time_to_detection,
# #     ylim = c(-5, 20)
# #   ) +
# #   ggtitle(
# #     label = "Surveillance trend",
# #     subtitle = "Time from modelled symptom onset to detection for locally-acquired cases"
# #   ) +
# #   xlab("Symptom onset date") +
# #   ylab("Days to case detection")
# # 
# # p_detection
# # 
# # save_ggplot("time_to_detection", multi = TRUE)
# 
# 
# 
# # compute overall ecdf
# optimal_isolation_ecdf <- ecdf(isolation_data$time_to_isolation)
# 
# surveillance_cdfs <- readRDS("outputs/delay_from_onset_cdfs.RDS")
# head(surveillance_cdfs)
# surveillance <- surveillance_effect(
#   dates = seq(
#     min(surveillance_cdfs$date),
#     max(surveillance_cdfs$date),
#     by = 1
#   ),
#   states = unique(surveillance_cdfs$state),
#   cdf = gi_cdf
# )
# 
# 
# # convert surveillance effect to weights (to represent how the effectiveness
# # of the contact tracing system changed over time) and compute a weighted
# # time-to-isolation cdf for each date and state
# isolation_cdfs <- surveillance_cdfs %>%
#   ungroup() %>%
#   rename(
#     surveillance_cdf = ecdf
#   ) %>%
#   mutate(
#     surveillance_effect = c(t(surveillance)),
#     ideal_isolation_ecdf = list(optimal_isolation_ecdf),
#     isolation_weight = 1 - surveillance_effect,
#     isolation_weight = isolation_weight / max(isolation_weight),
#     isolation_ecdf = mapply(
#       FUN = weight_ecdf,
#       surveillance_cdf,
#       ideal_isolation_ecdf,
#       1 - isolation_weight,
#       SIMPLIFY = FALSE
#     )
#   ) %>%
#   select(
#     date,
#     state,
#     ecdf = isolation_ecdf
#   )
# 
# # save this
# saveRDS(isolation_cdfs, "outputs/isolation_cdfs.RDS")
# 
# # compute the additional isolation effect as a check
# extra_isolation <- extra_isolation_effect(
#   dates = seq(
#     min(surveillance_cdfs$date),
#     max(surveillance_cdfs$date),
#     by = 1
#   ),
#   states = unique(surveillance_cdfs$state),
#   cdf = gi_cdf
# )
# 
# 
# # range of percentage reductions:
# # effect of detection and isolation of cases:
# range(100 * (1 - surveillance))
# # additional effect of isolating case contacts before they test positive
# range(100 * (1 - extra_isolation))
# #rage of overall effect
# range(100* (1 - surveillance * extra_isolation))
# 
# 
# isolation_effect <- bind_cols(
#   isolation_cdfs,
#   extra_isolation %>%
#     as_tibble(.name_repair = "unique") %>%
#     pivot_longer(
#       cols = everything(),
#       values_to = "isolation_effect"
#     ),
#   surveillance %>%
#     as_tibble(.name_repair = "unique") %>%
#     pivot_longer(
#       cols = everything(),
#       values_to = "surveillance_effect"
#     )
# ) %>%
#   dplyr::select(state, date, isolation_effect, surveillance_effect) %>%
#   mutate(
#     combined_effect = isolation_effect * surveillance_effect
#   )
# 
# 
# isolation_effect %>%
#   pivot_longer(
#     cols = ends_with("_effect"),
#     names_to = "effect_type",
#     values_to = "effect_value"
#   ) %>%
#   mutate(
#     effect_type = case_when(
#       effect_type == "combined_effect" ~ "TTIQ",
#       effect_type == "isolation_effect" ~ "Extra\nisolation",
#       TRUE ~ "Surveillance"
#     )
#   ) %>%
#   ggplot() +
#   geom_line(
#     aes(
#       x = date,
#       y = effect_value,
#       colour = effect_type
#     ),
#     size = 1
#   ) +
#   theme_classic() +
#   labs(
#     x = NULL,
#     y = "TTIQ effect",
#     col = "State"
#   ) +
#   scale_x_date(
#     breaks = "1 month",
#     date_labels = "%b %Y"
#   ) +
#   ggtitle(
#     label = "TTIQ effects",
#     subtitle = "Reduction in transmission due to surveillance and the isolation of cases"
#   ) +
#   cowplot::theme_cowplot() +
#   cowplot::panel_border(remove = TRUE) +
#     theme(
#       legend.position = "right",
#       strip.background = element_blank(),
#       strip.text = element_text(hjust = 0, face = "bold"),
#       axis.title.y.right = element_text(vjust = 0.5, angle = 90),
#       panel.spacing = unit(1.2, "lines"),
#       axis.text.x = element_text(size = 9, angle = 270)
#     ) +
#   scale_colour_manual(
#     values = c(
#       "purple",
#       yellow,
#       "coral"
#     )
#   ) +
#   scale_y_continuous(
#     position = "right",
#     limits = c(0.5, 1),
#     breaks = seq(0, 1, by = 0.1)
#   ) +
#   facet_wrap(
#     ~ state,
#     ncol = 2
#   )
#   
# 
# save_ggplot("extra_isolation_effect.png", multi = TRUE)
# 
# write_csv(
#   isolation_effect,
#   file = "outputs/ttiq_effects.csv"
# )
#   
