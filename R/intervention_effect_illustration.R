# plot the combined impacts of vaccination and public health and social measures
# (PHSM) on control (reducing transmission potential to <1 of COVID-19)


# # proportional intervention effects in each scenario
# contributions <- tribble(
#   ~scenario, ~surv, ~micro, ~ttiq, ~macro, ~vacc,
#   "Vic 2nd wave", 0.1, 0.1, 0.4, 0.4, 0,
#   "Vic 2nd wave with vaccination", 0.1, 0, 0, 0, 0.9,
# )
#
# intervention_names <- names(contributions)[-1]
# # colours <- c("yellow", "blue", "lavender", "purple", "green")
# colours <- RColorBrewer::brewer.pal(length(intervention_names), "Set3")
#
# # R0 and Reff for each scenario
# effects <- tribble(
#   ~scenario, ~r0, ~reff,
#   "Vic 2nd wave", 3, 0.8,
#   "Vic 2nd wave with vaccination", 3, 0.9
# )
#
# # convert these into cumulative values to plot for each scenario
# # bang bang wasn't working properly here, so I fudged it.
# cumulative_contributions <- contributions
# for (i in seq_along(intervention_names[-1])) {
#   to_update <- intervention_names[i + 1]
#   to_add <- intervention_names[i]
#   cumulative_contributions <- cumulative_contributions %>%
#     mutate_at(
#       to_update,
#       `+`,
#       e2 = .[[to_add]]
#     )
# }
#
# # combine with Reff to get values for plotting
# plot_data <- cumulative_contributions %>%
#   left_join(effects, by = "scenario") %>%
#   mutate_at(
#     intervention_names,
#     ~. * (r0 - reff) + reff
#   ) %>%
#   select(scenario, reff, !!!intervention_names, -r0)

summarise_samples <- function(samples_file) {
  delta_summary <-
    samples_file %>%
    read_csv(
      col_types = cols(
        .default = col_double(),
        date = col_date(),
        state = col_character(),
        date_onset = col_date()
      )
    ) %>%
    pivot_longer(
      cols = !(date:date_onset),
      names_to = "sim",
      values_to = "tp"
    ) %>%
    group_by(
      date, state
    ) %>%
    summarise(
      tp = mean(tp),
      .groups = "drop"
    )
}

# extract R0 from a summarised trajectory of TP values, by taking TP at the
# baseline time period (limited surveillance effect, but no behavioural change),
# and dividing out the surveillance effect, and the extra isolation effect, if
# that was used to calculate this TP trajectory
get_r0 <- function(sample_summary, use_extra_effect = TRUE) {
  
  start <- sample_summary %>%
    filter(
      state == "ACT",
      date == as_date("2020-01-08")
    )

  start %>%
    mutate(
      surveillance = surveillance_effect(
        dates = start$date,
        state = start$state,
        cdf = gi_cdf
      )[1, 1],
      extra_isolation = extra_isolation_effect(
        dates = start$date,
        state = start$state,
        cdf = gi_cdf
      )[1, 1],
      correction = surveillance * extra_isolation ^ use_extra_effect,
      tp_corrected = tp / correction
    ) %>%
    pull(
      tp_corrected
    )
}

# make a washed-out colour, by a vector of amounts between 0 and 1
washout <- function(colour, amount = 0.7) {

  stopifnot(
    all(amount >= 0) & all(amount <= 1)
  )

  n <- 1000
  indices <- pmax(1, ceiling(amount * n))

  palette <- colorRampPalette(c(colour, "white"))
  palette(n)[indices]

}


add_context_hline <- function(
  p,
  at,
  label,
  colour = grey(0),
  linetype = 2,
  text_size = 2.5
) {
  p +
    geom_hline(
      yintercept = at,
      col = colour,
      linetype = linetype
    ) +
    annotate(
      "text",
      label = label,
      x = -0.5,
      y = at,
      hjust = 0,
      vjust = -0.5,
      col = colour,
      size = text_size
    )
}

control_plot_theme <- function() {
  cowplot::theme_cowplot() +
    # turn off the x axis and add some space for annotation on RHS
    theme(
      plot.margin = unit(
        c(1, 1, 0, 1),
        "line"
      ),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.title.y.right = element_text(
        size = 12,
        margin = margin(l = 10),
      )
    )
}

control_base_plot <- function(
  data,
  ylab = "Transmission potential"
) {
  data %>%
    ggplot(
      aes(
        x = scenario,
        middle = as.numeric(NA),
        ymin = as.numeric(NA),
        ymax = as.numeric(NA),
        width = 0.6
      )
    ) +
    scale_x_discrete() +
    xlab("") +
    scale_y_continuous(
      position = "right",
      breaks = c(0.5, 1, 2, 4, 6, 8),
      trans = 'log'
    ) +
    coord_cartesian(clip = "off") +
    ylab(ylab) +
    control_plot_theme()
}

add_single_box <- function(
  p,
  top,
  bottom,
  text_main = "",
  use_reduction_text = FALSE,
  only_scenarios = c(),
  text_size = 3,
  box_colour = grey(0.9),
  text_colour = grey(0.3),
  border_colour = grey(0.6)
) {

  top <- enquo(top)
  bottom <- enquo(bottom)
  p <- p +
    geom_boxplot(
      aes(
        lower = !!bottom,
        upper = !!top
      ),
      stat = "identity",
      fill = box_colour,
      col = border_colour
    )
  
  if (use_reduction_text) {
  
    # restriction labels
    p <- p + geom_text(
      aes(
        label = ifelse(
          scenario %in% only_scenarios,
          sprintf(
            "%s\n%s\n coverage",
            text_main,
            scenario
          ),
          NA
        ),
        y = !!bottom * (!!top / !!bottom) ^ 0.5  # (midpoint on log scale!)
      ),
      size = text_size,
      col = text_colour
    )
  } else {
    p <- p + geom_text(
      aes(
        label = ifelse(
          scenario %in% only_scenarios,
          text_main,
          NA
        ),
        y = !!bottom * (!!top / !!bottom) ^ 0.5  # (midpoint on log scale!)
      ),
      size = text_size,
      col = text_colour
    )
  }
  
  p
  
}

add_stacked_box <- function(
  p,
  bottom,  # variable for bottom of the box
  top,  # variable for top of the box
  reference,  # variable against which to calculate % reduction of 'bottom'
  text_main = "",
  text_size = 3,
  only_scenarios = c(),
  box_colour = grey(0.9),
  border_colour = grey(0.6),
  text_colour = grey(0.3),
  use_reduction_text = FALSE
) {

  bottom <- enquo(bottom)
  top <- enquo(top)
  reference <- enquo(reference)

  p <- p +
    geom_boxplot(
      aes(
        lower = !!bottom,
        upper = !!top,
      ),
      stat = "identity",
      fill = box_colour,
      col = border_colour
    )
  
  if (use_reduction_text) {
    p <- p + 
      geom_text(
        aes(
          label = ifelse(
            scenario %in% only_scenarios,
            sprintf(
              "%s\n%i%s",
              text_main,
              round(100 * (1 - !!bottom/!!reference)),
              "% lower TP"
            ),
            NA
          ),
          y = !!bottom * (!!top /!!bottom) ^ 0.5  # (midpoint on log scale!)
        ),
        size = text_size,
        col = text_colour
      )
  } else {
    p <- p + 
      geom_text(
        aes(
          label = ifelse(
            scenario %in% only_scenarios,
            text_main,
            NA
          ),
          y = !!bottom * (!!top /!!bottom) ^ 0.5  # (midpoint on log scale!)
        ),
        size = text_size,
        col = text_colour
      )
  }
  
  p

}

add_arrow <- function(
  p,
  r0,
  end = 1.05,
  at = 0.35,
  size = 3,
  colour = grey(0.8)
) {

  r0 <- enquo(r0)
  p +
    geom_segment(
      aes(
        x = at,
        xend = at,
        y = max(!!r0),
        yend = end
      ),
      data = p$data,
      size = size,
      linejoin = "mitre",
      colour = colour,
      arrow = arrow(
        type = "closed",
        length = unit(7.5, "point")
      )
    )
}

# boxplot-y figure on intervention efects on R effective
library(tidyverse)

delta_summary <- summarise_samples("~/Desktop/delta_r_eff_1_local_samples.csv")
non_voc_summary <- summarise_samples("~/Desktop/non_voc_tp_samples.csv")

source("R/functions.R")

# compute R0 for each variant (WT was computed w/out extra isolation effect)
r0 <- list(
  delta = get_r0(delta_summary),
  non_voc = get_r0(non_voc_summary, use_extra_effect = FALSE)
)

tps <- read_csv("~/Desktop/tp_scenarios_draft_2021-07-27.csv") %>%
  filter(
    ttiq %in% c("partial", "optimal"),
    baseline_type == "standard",
    vacc_schoolkids == FALSE,
    vacc_relative_efficacy == 1,
    az_age_cutoff == 60,
    az_dose_gap == "12 weeks",
    priority_order == "Random"
  ) %>%
  mutate(
    scenario = paste0(100 * vacc_coverage, "%"),
    r0 = r0$delta
  )
  
colours <- RColorBrewer::brewer.pal(3, "Set2")

baseline_colour <- washout(colours[2], 0.8)
vaccine_colour <- washout(colours[3], 0.5)
vaccine_dark_colour <- washout(colours[3], 0.3)
phsm_colours <- washout(colours[1], c(0.5, 0.25, 0.1))
phsm_dark_colours <- washout(colours[1], c(0.4, 0.15, 0))


border_colour <- grey(0.6)
r0_colour <- grey(0.5)
label_colour <- grey(0.3)

text_size <- 2.5

# make plots with both optimal and partial TTIQ
for (ttiq_plot in c("partial", "optimal")) {
  tps %>%
    filter(
      ttiq == ttiq_plot
    ) %>%
    control_base_plot() %>%
    add_context_hline(
      label = "non-VOC R0",
      at = r0$non_voc,
      col = r0_colour,
      linetype = 3,
      text_size = text_size
    ) %>%
    add_context_hline(
      label = "Control",
      at = 1,
      linetype = 2,
      text_size = text_size * 1.3
    ) %>%
    # add the vaccination + ttiq effect as a box
    add_single_box(
      top = r0,
      bottom = tp_baseline,
      box_colour = baseline_colour,
      only_scenarios = "50%",
      text_main = paste0(
        "baseline\nPHSM\n&\n",
        ttiq_plot,
        "\nTTIQ"
      )
    ) %>%
    add_single_box(
      top = tp_baseline,
      bottom = tp_baseline_vacc,
      box_colour = vaccine_colour,
      text_main = "vaccination",
      only_scenarios = unique(tps$scenario),
      use_reduction_text = TRUE
    ) %>%
    add_stacked_box(
      top = tp_baseline_vacc,
      bottom = tp_low_vacc,
      reference = tp_baseline_vacc,
      text_main = "low\nPHSM",
      only_scenarios = "50%",
      box_colour = phsm_colours[1]
    ) %>%
    add_stacked_box(
      top = tp_low_vacc,
      bottom = tp_medium_vacc,
      reference = tp_baseline_vacc,
      text_main = "medium\nPHSM",
      only_scenarios = "50%",
      box_colour = phsm_colours[2]
    ) %>%
    add_stacked_box(
      top = tp_medium_vacc,
      bottom = tp_high_vacc,
      reference = tp_baseline_vacc,
      text_main = "high\nPHSM",
      only_scenarios = "50%",
      box_colour = phsm_colours[3]
    ) %>%
    add_context_hline(
      label = "Delta R0",
      at = r0$delta,
      linetype = 2,
      text_size = text_size * 1.3
    ) %>%
    add_arrow(r0)
  
  ggsave(
    paste0(
      "~/Desktop/phsm_plot_",
      ttiq_plot,
      ".png"
    ),
    width = 8,
    height = 6,
    bg = "white"
  )
}
