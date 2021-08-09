
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

tps <- read_csv(
  "~/Desktop/tp_scenarios_draft_2021-07-27.csv",
  col_types = cols(
    .default = col_double(),
    ttiq = col_character(),
    baseline_type = col_character(),
    vacc_schoolkids = col_logical(),
    priority_order = col_character(),
    az_dose_gap = col_character()
  )) %>%
  filter(
    ttiq %in% c("partial", "optimal"),
    baseline_type == "standard",
    vacc_schoolkids == FALSE,
    vacc_relative_efficacy == 1,
    az_age_cutoff == 60,
    az_dose_gap == "12 weeks",
    priority_order %in% c("Random", "Random, with phasing")
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
  for (priority_order_plot in c("Random", "Random, with phasing")) {
    tps %>%
      filter(
        ttiq == ttiq_plot,
        priority_order == priority_order_plot
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
        "_",
        priority_order_plot,
        ".png"
      ),
      width = 8,
      height = 6,
      bg = "white"
    )
  }
}
