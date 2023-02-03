source("R/lib.R")

source("R/functions.R")

#format_raw_survey_data() # only need to run this line if
# microdistancing isn't done
# to process all the data from painted dog

get_mask_data_all <- function (all_surv = NULL) {
  if(is.null(all_surv)){
    all_surv <- parse_all_surveys()
  }
  
  mask_data_all <- all_surv  %>%
    select(
      -starts_with("contact")
    ) %>%
    pivot_longer(
      cols = c(face_covering),
      names_to = "question",
      values_to = "response"
    ) %>%
    select(-question) %>%
    filter(
      !is.na(response),
      !is.na(state)
    ) %>%
    group_by(state, wave_date, response) %>%
    summarise(count = n()) %>%
    ungroup %>%
    complete(state, wave_date, response, fill = list(count = 0)) %>%
    mutate(
      response = factor(
        response,
        levels = c("No", "Rarely", "Sometimes", "Often", "Always")
      )
    ) %>%
    arrange(state, wave_date, response) %>%
    group_by(state, wave_date) %>%
    mutate(
      n = sum(count),
      proportion = count/n
    ) %>%
    ungroup %>%
    select(-n)
  
  return(mask_data_all)
}

mask_data_all <- get_mask_data_all()

ggplot(mask_data_all) + 
  geom_bar(
    aes(
      x = wave_date,
      y = proportion,
      col = response,
      fill = response
    ),
    stat = "identity"
  ) +
  facet_wrap(
    ~ state,
    ncol = 2
  ) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90),
    panel.spacing = unit(1.2, "lines"),
    axis.text.x = element_text(size = 7),
    axis.title.x = element_blank()#,
    #legend.position = "bottom",
  ) +
  labs(
    y = "Proportion of respondents answering\n 'Do you wear a face covering in public ...'",
    title = "Mask-wearing trends",
    subtitle = "Raw proportions of weekly responses.",
    colour = "Response",
    fill = "Response"
  ) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b%y")


save_ggplot("masks_all_responses.png")



hd <- hygiene_data()

mask_data_yn <- hd %>%
  filter(question == "Face covering") %>%
  select(-question) #%>%
  # group_by(state, wave_date) %>%
  # summarise(
  #   count = sum(count),
  #   respondents = sum(respondents)
  # )


min_date <- min(mask_data_yn$date)
max_date <- max(mask_data_yn$date)

all_dates <- seq(min_date, max_date, by = 1)

min_data_date <- min(mask_data_yn$date)
max_data_date <- max(mask_data_yn$date)


intervention_steps <- interventions(
  end_dates = TRUE#,
  # exclude_after = "2021-10-21"
) %>%
  filter(date <= max_data_date) %>%
  # no survey data from during the TAS lockdown in these dates so not possible
  # to fit effect of this lockdown, and therefore excluding this intervention
  filter(!(state == "TAS" & date >= "2021-10-16" & date <= "2021-10-19")) %>%
  mutate(
    intervention_id = paste0(
      "intervention_",
      match(date, unique(date))
    )
  ) %>%
  group_by(intervention_id, state) %>%
  do(
    tibble(
      date = all_dates,
      intervention_effect = as.numeric(all_dates >= .$date)
    )
  ) %>%
  group_by(state, date) %>%
  summarise(
    intervention_stage = sum(intervention_effect),
    .groups = "drop"
  ) %>%
  mutate(
    intervention_stage = factor(intervention_stage)
  )


min_intervention_stage <- intervention_steps %>%
  filter(date == min_data_date) %>%
  dplyr::rename(min_intervention_stage = intervention_stage) %>%
  dplyr::select(-date)

max_intervention_stage <- intervention_steps %>%
  filter(date == max_data_date) %>%
  dplyr::rename(max_intervention_stage = intervention_stage) %>%
  dplyr::select(-date)  


mask_pred_data <- expand_grid(
  date = seq.Date(from = min_data_date, to = max_data_date, by = 1),
  state = unique(mask_data_yn$state)
)


df_fit <- mask_data_yn %>%
  left_join(
    intervention_steps,
    by = c("state", "date")
  )%>%
  dplyr::select(
    state,
    date,
    count,
    respondents,
    intervention_stage
  ) %>%
  nest(
    fit_dat = c(
      date,
      count,
      respondents,
      intervention_stage
    )
  )



df_pred <- mask_pred_data %>%
  left_join(
    intervention_steps,
    by = c("state", "date")
  ) %>%
  left_join(
    min_intervention_stage,
    by = "state"
  ) %>%
  left_join(
    max_intervention_stage,
    by = "state"
  ) %>%
  mutate(
    intervention_stage = case_when(
      is.na(intervention_stage) & date < min_data_date ~ min_intervention_stage,
      is.na(intervention_stage) & date > max_data_date ~ max_intervention_stage,
      state == "VIC" & intervention_stage == 4 ~  factor(5, levels = levels(intervention_stage)),
      TRUE ~ intervention_stage
    )
  ) %>%
  dplyr::select(
    state,
    date,
    intervention_stage
  ) %>%
  nest(
    pred_dat = c(
      date,
      intervention_stage
    )
  )



df_mask <- full_join(
  df_fit,
  df_pred,
  by = "state"
)

x <- 6
fit_dat <- df_mask$fit_dat[[x]]
pred_dat <- df_mask$pred_dat[[x]]

fit_mask_gam <- function(
  fit_dat,
  pred_dat
){
  
  respondents <- fit_dat$respondents
  count <- fit_dat$count
  date <- fit_dat$date
  intervention_stage <- fit_dat$intervention_stage
  
  date_num <- as.numeric(date - min(date))
  
  
  if(length(unique(fit_dat$intervention_stage)) == 1) {
    m <- mgcv::gam(
      cbind(count, I(respondents - count)) ~ s(date_num),
      select = TRUE,
      family = stats::binomial,
      optimizer = c("outer","optim")
    )
  } else {
    m <- mgcv::gam(
      cbind(count, I(respondents - count)) ~ s(date_num)  + intervention_stage,
      select = TRUE,
      family = stats::binomial,
      optimizer = c("outer","optim")
    )
  }

  
  pred_dat$date_num <- as.numeric(pred_dat$date - min(date))
  
  #pred <- predict(m, se.fit = TRUE, type = "link")
  pred <- predict(
    object = m,
    newdata = pred_dat,
    se.fit = TRUE,
    type = "link"
  )
  
  quantile95 <- qnorm(0.95)
  quantile75 <- qnorm(0.75)
  ci_90_hi <- pred$fit + (quantile95 * pred$se.fit)
  ci_90_lo <- pred$fit - (quantile95 * pred$se.fit)
  ci_50_hi <- pred$fit + (quantile75 * pred$se.fit)
  ci_50_lo <- pred$fit - (quantile75 * pred$se.fit)
  
  fitted <- m$family$linkinv(pred$fit)
  ci_90_hi <- m$family$linkinv(ci_90_hi)
  ci_90_lo <- m$family$linkinv(ci_90_lo)
  ci_50_hi <- m$family$linkinv(ci_50_hi)
  ci_50_lo <- m$family$linkinv(ci_50_lo)
  
  
  
  tibble(
    date = pred_dat$date,
    mean = fitted ,
    ci_90_lo,
    ci_50_lo,
    ci_50_hi,
    ci_90_hi
  )
  
}



mask_fit <- mapply(
  FUN = fit_mask_gam,
  fit_dat = df_mask$fit_dat,
  pred_dat = df_mask$pred_dat,
  SIMPLIFY = FALSE
)


pred_plot <- df_mask %>%
  mutate(fit = mask_fit) %>% 
  unnest(fit) %>%
  dplyr::select(-fit_dat, -pred_dat)


line_df <- pred_plot %>%
  mutate_at(
    vars(mean, ci_90_lo, ci_90_hi, ci_50_lo, ci_50_hi),
    ~ . * 100
  ) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  mutate(type = "Nowcast")



point_df <- mask_data_yn %>%
  group_by(state, wave_date) %>%
  summarise(
    count =  sum(count),
    respondents = sum(respondents)
  ) %>%
  ungroup() %>%
  mutate(
    proportion = count / respondents,
    percentage = proportion * 100
  ) %>%
  rename(date = wave_date) %>%
  mutate(type = "Nowcast")

# Compute confidence intervals for the proportions for plotting. Need to fudge
# the sample size for one survey round with 100% adherence on a small sample
pred <- point_df %>%
  mutate(
    id = factor(row_number()),
    respondents = ifelse(respondents == count,
                         respondents + 1,
                         respondents)
  ) %>%
  glm(cbind(count, respondents - count) ~ id,
      data = .,
      family = stats::binomial) %>%
  predict(se.fit = TRUE)

# Monte Carlo integration based on normal approximation to logit-probability
logit_sims <- replicate(
  10000,
  rnorm(length(pred$fit),
        pred$fit,
        pred$se.fit)
)

p_sims <- plogis(logit_sims)
estimate <- rowMeans(p_sims)
cis <- t(apply(
  X = p_sims,
  MARGIN = 1,
  FUN = quantile,
  c(0.025, 0.975)
))

point_df <- point_df %>%
  mutate(
    percentage = estimate * 100,
    lower = cis[, 1] * 100,
    upper = cis[, 2] * 100
  )

# # save these fits for plotting later
# module(line_df, point_df) %>%
#   saveRDS("outputs/mask_plotting_data.RDS")



base_colour <- "#98F5FF"

p <- ggplot(line_df) +
  
  aes(date, mean, fill = type) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b%y") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions(),
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
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 7)) +
  
  # add empirical percentages
  geom_point(
    aes(date, percentage),
    data = point_df,
    size = 2,
    pch = "_"
  ) +
  
  geom_errorbar(
    aes(date, percentage, ymin = lower, ymax = upper),
    data = point_df,
    size = 1,
    alpha = 0.2,
    width = 0
  ) +
  
  # and titles  
  ggtitle(
    label = "Mask-wearing trend",
    subtitle = "Calibrated against self-reported mask-wearing"
  ) +
  ylab("Estimate of percentage 'always' wearing face covering")

p


save_ggplot("mask_wearing_always.png")

p <- ggplot(line_df) +
  
  aes(date, mean, fill = type) +
  
  xlab(element_blank()) +
  
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(position = "right") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b%y") +
  scale_alpha(range = c(0, 0.5)) +
  scale_fill_manual(values = c("Nowcast" = base_colour)) +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions(),
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
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        panel.spacing = unit(1.2, "lines"),
        axis.text.x = element_text(size = 7)) +
  
  # and titles  
  ggtitle(
    label = "Mask-wearing trend",
    subtitle = "Calibrated against self-reported mask-wearing"
  ) +
  ylab("Estimate of percentage 'always' wearing face covering")

p


save_ggplot("mask_wearing_always_line_only.png")
