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

parse_doh_survey <- function(filename) {
  full <- filename %>%
    read_csv(
      col_types = cols(
        .default = col_character(),
        S1 = col_double(),
        Q138 = col_double(),
        Q166_1 = col_double(),
        Q166_4 = col_double(),
        Q167_1 = col_double(),
        Q168_2 = col_double(),
        Q169_1 = col_double()
      )
    ) %>%
    mutate(
      wave = wave_from_file(filename),
      state = abbreviate_states(S3),
      city = Location == "Major city",
      parent = Q39 %in% parenty_households,
      distancing_any = NA,
      cough_any = NA,
      mask = NA,
      survey = "doh",
      date = as.Date(StartDate, format = "%Y%m%d"),
    )
  
  # add face covering data if it is there
  if ("Q222" %in% names(full)) {
    full <- full %>%
      mutate(face_covering = Q222)
  } else {
    full <- full %>%
      mutate(face_covering = NA)
  }
  
  if ("Q224" %in% names(full)) {
    full <- full %>%
      mutate(vaccinated = Q224)
  } else {
    full <- full %>%
      mutate(vaccinated = NA)
  }
  
  if ("Q225B" %in% names(full)) {
    full <- full %>%
      mutate(guidelines_given_vaccinated = Q225B)
  } else {
    full <- full %>%
      mutate(guidelines_given_vaccinated = NA)
  }
 
  
  # likelihood of seeking test given symptoms
  # this questions starts 2021-04-19, however from
  # 2021-11-08 respondents are given a randomly given
  # one of 3 sets of symptoms. Before this date we code the
  # symptom set as 4
  if ("Q226" %in% names(full)) {
    full <- full %>%
      mutate(test_seeking = Q226)
    if("Q226_Symptoms" %in% names(full)){
      full <- full %>%
        mutate(test_seeking_set = Q226_Symptoms)
    } else{
      full <- full %>%
        mutate(test_seeking_set = "4")
    }
  } else {
    full <- full %>%
      mutate(
        test_seeking = NA,
        test_seeking_set = NA
      )
  }
  
  if ("Q228_1" %in% names(full)) {
    full <- full %>%
      mutate(
        symptoms_cough = Q228_1,
        symptoms_fever = Q228_2,
        symptoms_difficultybreathing = Q228_3,
        symptoms_sorethroat = Q228_4,
        symptoms_tiredness = Q228_5,
        symptoms_jointaches = Q228_6,
        symptoms_headache = Q228_7,
        symptoms_runnynose = Q228_8,
        symptoms_tastesmellchange = Q228_9,
        symptoms_nauseavomit = Q228_10,
        symptoms_chills = Q228_11,
        symptoms_none = Q228_99,
        tested = Q229,
        why_tested_symptoms = Q230_1,
        why_tested_contact = Q230_2,
        why_tested_job = Q230_3,
        why_tested_other = Q230_98,
        why_tested_other_specify = Q230_98_OTH
      )
  } else {
    full <- full %>%
      mutate(
        symptoms_cough = NA,
        symptoms_fever = NA,
        symptoms_difficultybreathing = NA,
        symptoms_sorethroat = NA,
        symptoms_tiredness = NA,
        symptoms_jointaches = NA,
        symptoms_headache = NA,
        symptoms_runnynose = NA,
        symptoms_tastesmellchange = NA,
        symptoms_nauseavomit = NA,
        symptoms_chills = NA,
        symptoms_none = NA,
        tested = NA,
        why_tested_symptoms = NA,
        why_tested_contact = NA,
        why_tested_job = NA,
        why_tested_other = NA,
        why_tested_other_specify = NA
      )
  }
  
  if("Q231_1" %in% names(full)){
    full <- full %>%
      mutate(
        test_type_pcr = Q231_1,
        test_type_rat = Q231_2,
        test_type_unknown = Q231_3,
        test_result_pcr = Q231a,
        test_result_rat = Q231b
      )
  } else {
    full <- full %>%
      mutate(
        test_type_pcr = NA,
        test_type_rat = NA,
        test_type_unknown = NA,
        test_result_pcr = NA,
        test_result_rat = NA
      )
  }
  
  if("Q232" %in% names(full)){
    full <- full %>%
      mutate(
        report_rat = Q232
      )
  } else {
    full <- full %>%
      mutate(
        report_rat = NA
      )
  }
  
  if ("Q233" %in% names(full)) {
    full <- full %>%
      mutate(
        informed_contact = Q233,
        response_to_informed_contact = Q234,
        action_given_symptoms = Q235
      )
  } else {
    full <- full %>%
      mutate(
        informed_contact = NA,
        response_to_informed_contact = NA,
        action_given_symptoms = NA
      )
  }
  
  survey_results <- full %>%
    select(
      wave,
      state,
      gender = S2,
      age = S1,
      vulnerable = Q75,
      age_groups = AgeBracket,
      city,
      location = Location,
      postcode = Q37,
      household = Q39,
      income = Q42,
      isolating = Q61,
      how_likely_to_catch = Q14,
      attitude_severe = Q56,
      work_location = Q101,
      parent,
      employment = Q38,
      phys_contact = Q109,
      phys_distance = Q65,
      wash_hands = Q110,
      cough_any,
      cough = Q111,
      mask,
      face_covering,
      vaccinated,
      guidelines_given_vaccinated,
      test_seeking,
      test_seeking_set,
      symptoms_cough,
      symptoms_fever,
      symptoms_difficultybreathing,
      symptoms_sorethroat,
      symptoms_tiredness,
      symptoms_jointaches,
      symptoms_headache,
      symptoms_runnynose,
      symptoms_tastesmellchange,
      symptoms_nauseavomit,
      symptoms_chills,
      symptoms_none,
      tested,
      why_tested_symptoms,
      why_tested_contact,
      why_tested_job,
      why_tested_other,
      why_tested_other_specify,
      test_type_pcr,
      test_type_rat,
      test_type_unknown,
      test_result_pcr,
      test_result_rat,
      report_rat,
      informed_contact,
      response_to_informed_contact,
      action_given_symptoms,
      contact_num = Q138,
      contacts_under18 = Q166_1,
      contacts_18to39 = Q166_2,
      contacts_40to59 = Q166_3,
      contacts_60plus = Q166_4,
      contacts_ageunsure = Q166_5,
      contacts_phys = Q167_1,
      contacts_close = Q167_2,
      contacts_notclose = Q167_3,
      contacts_physdontknow = Q167_4,
      contacts_home = Q168_1,
      contacts_work = Q168_2,
      contacts_worship = Q168_3,
      contacts_school = Q168_4,
      contacts_shop = Q168_5,
      contacts_cafe = Q168_6,
      contacts_sport = Q168_7,
      contacts_public = Q168_8,
      contacts_other = Q168_9,
      contacts_cantremember = Q168_10,
      contacts_5min = Q169_1,
      contacts_5to30min = Q169_2,
      contacts_30to90min = Q169_3,
      contacts_90minto3hrs = Q169_4,
      contacts_3hrsplus = Q169_5,
      contacts_timedontknow = Q169_6,
      date,
      survey
    ) %>%
    mutate_at(
      vars(vulnerable, phys_contact),
      ~yesno_to_logical(.)
    ) %>%
    mutate_at(
      vars(starts_with("contacts_")),
      ~as.numeric(.)
    ) %>%
    mutate_at(
      vars(
        starts_with("symptoms_"),
        starts_with("why_tested_"),
        starts_with("test_type_")
      ),
      ~quoted_to_logical(.)
    )
    
  return(survey_results)
}

quoted_to_logical <- function(x) {
  case_when(
    x == "quoted" ~ TRUE,
    x == "not quoted" ~ FALSE,
    TRUE ~ NA
  )
}

survey_question_first_asked <- function(dir = "data/survey_raw") {
  
  files <- dir %>%
    list.files(
      pattern = ".csv$",
      full.names = TRUE
    )
  
  filenames <- dir %>%
    list.files(
      pattern = ".csv$",
      full.names = FALSE
    )
  
  
  
  survey_no <- sub(
    pattern = "\\..*",
    replacement = "",
    x = filenames
  ) %>%
    as.numeric
  
  survey_date <- sub(
    pattern = ".*Tracker ",
    replacement = "",
    x = filenames
  ) %>%
    sub(
      pattern = ".*cases ",
      replacement = "",
      x = .
    ) %>%
    sub(
      pattern = ".csv",
      replacement = "",
      x = .
    ) %>%
    dmy
  
  questions_asked <- lapply(
    X = files,
    FUN = function(x){
      read_csv(x) %>%
        colnames
    }
  )
  
  # ql <- sapply(questions_asked, length)
  # ql[order(survey_no)]
  
  mxsn <- which.max(survey_no)
  questions <- questions_asked[[mxsn]]
  
  asked <- lapply(
    X = questions_asked,
    FUN = function(x, y){
      z <- y %in% x
      names(z) <- y
      as_tibble_row(z)
    },
    y = questions
  ) %>%
    bind_rows %>%
    mutate(
      survey_no,
      survey_date = survey_date
    ) %>%
    select(
      survey_no,
      survey_date,
      everything()
    ) %>%
    arrange(survey_date)
  
  first <- sapply(
    X = asked,
    FUN = function(x,y){
      min(which(x == TRUE))
    },
    y = survey_date
  )
  
  first_asked <- tibble(
    question = questions,
    first_asked = asked$survey_date[first[-c(1:2)]]
  )
  
  first_asked
  
}

sqfa <- survey_question_first_asked()

single_reponse_summary <- function(data, var){
  
  data %>%
    select(
      #wave,
      wave_date,
      state,
      response = {{var}}
    ) %>% 
    filter(
      !is.na(response),
      !is.na(state)
    )  %>%
    group_by(state, wave_date, response) %>%
    summarise(count = n()) %>%
    ungroup %>%
    complete(state, wave_date, response, fill = list(count = 0)) %>%
    arrange(state, wave_date, response) %>%
    group_by(state, wave_date) %>%
    mutate(
      n = sum(count),
      proportion = count/n
    ) %>%
    ungroup %>%
    select(-n)
  
}

plot_single_response_stack <- function(
    data,
    title = NULL,
    subtitle = NULL,
    ylim = NULL,
    date_breaks = NULL,
    date_labels = NULL
){
  
  if(is.null(date_breaks)){
    date_breaks <- "4 month"
  }
  
  if(is.null(date_labels)){
    date_labels <- "%b%y"
  }
  
  ggplot(data) + 
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
      y = "Raw proportions of weekly responses",
      title = title,
      subtitle = subtitle,
      caption = "This is a summary of raw data.\nQuestion and responses here may be paraphrased from actual questions asked.",
      colour = "Response",
      fill = "Response"
    ) +
    scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
}

plot_single_response_line <- function(
    data,
    title = NULL,
    subtitle = NULL,
    ylim = NULL,
    date_breaks = NULL,
    date_labels = NULL
){
  
  if(is.null(date_breaks)){
    date_breaks <- "4 month"
  }
  
  if(is.null(date_labels)){
    date_labels <- "%b%y"
  }
  
  p <- ggplot(data) + 
    geom_line(
      aes(
        x = wave_date,
        y = proportion,
        col = response,
        alpha = 0.3
      )
    ) +
    geom_smooth(
      aes(
        x = wave_date,
        y = proportion,
        col = response
      ),
      se = FALSE,
      method = "gam"
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
      y = "Smoothed and raw (faint) proportions of weekly responses",
      title = title,
      subtitle = subtitle,
      caption = "This is a summary of raw data.\nSmoothed lines are to aid visual purposes only, they do not represent a model fit.\nQuestion and responses here may be paraphrased from actual questions asked.",
      colour = "Response",
      fill = "Response"
    ) +
    scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    guides(alpha = "none")
  
  if(!is.null(ylim))
   p <- p +
      ylim(ylim)
  p
}


#### masks

all_surveys <- parse_all_surveys()

mask_data_all <- get_mask_data_all(all_surv = all_surveys)

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



# base_colour <- "#98F5FF" # surgical mask colour aka cadetblue1

base_colour <- "#FF00FF"

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

#### single response data

all_survey_results <- parse_all_surveys()


save_date <- max(all_survey_results$wave_date) %>% 
  format("%Y%m%d")
## single responses to look at 

# isolating - q61
# Which of the following best describes the approach you have been taking in the last week with respect to COVID-19?

isolating_summary <- single_reponse_summary(all_survey_results, isolating) %>%
  mutate(
    response = case_when(
      # response == is_response_1 ~ "Completely\nisolating",
      # response == "I am completely isolating – not leaving home for any reason" ~ "Completely\nisolating",
      str_starts(response, "I am completely isolating") ~ "Completely\nisolating",
      response == "I am not trying to stay home" ~ "Not staying\nhome",
      response == "I am staying home as much as I can, and only going out when I absolutely have to" ~ "Mostly\nisolating",
      response == "I am still choosing to leave my home on a regular basis, but I am going out less than before the Coronavirus outbreak" ~ "Staying home\na little"
    )
  ) %>%
  group_by(state, wave_date, response) %>%
  summarise(
    count = sum(count),
    proportion = sum(proportion)
  ) %>%
  mutate(
    response = factor(
      response,
      levels = c(
        "Not staying\nhome",
        "Staying home\na little",
        "Mostly\nisolating",
        "Completely\nisolating"
      )
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  isolating_summary,
  title = "Staying home behaviour",
  subtitle = "Proportion of respondents answering 'What best describes approach you\nhave been taking to COVID-19?'"
)

save_ggplot(
  sprintf(
    "isolating_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  isolating_summary,
  title = "Staying home behaviour",
  subtitle = "Proportion of respondents answering 'What best describes approach you\nhave been taking to COVID-19?'"
)

save_ggplot(
  sprintf(
    "isolating_line_%s.png",
    save_date
  )
)

# phys_distance - q65
# Are you staying 1.5m away from people outside of your household? 

phys_distance_summary <- single_reponse_summary(all_survey_results, phys_distance) %>%
  mutate(
    response = factor(
      response,
      levels = c("No", "Rarely", "Sometimes", "Often", "Always")
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  phys_distance_summary,
  title = "Physical distancing behaviour",
  subtitle = "Proportion of respondents answering 'Are you staying 1.5m away from people\noutside of your household?'"
)

save_ggplot(
  sprintf(
    "phys_distancing_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  phys_distance_summary,
  title = "Physical distancing behaviour",
  subtitle = "Proportion of respondents answering 'Are you staying 1.5m away from people\noutside of your household?'"
)

save_ggplot(
  sprintf(
    "phys_distancing_line_%s.png",
    save_date
  )
)

# phys_contact - q109
# In the last week, have you had physical contact with a person who doesn’t live with you? (e.g. shaken hands, hugged, body contact during sports)? 
phys_contact_summary <- single_reponse_summary(all_survey_results, phys_contact) %>%
  mutate(
    response = case_when(
      response ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  phys_contact_summary,
  title = "Physical contact",
  subtitle = "Proportion of respondents answering 'In the last week, have you had physical\ncontact with a person who doesn’t live with you? (e.g. shaken hands, hugged,\nbody contact during sports)'"
)

save_ggplot(
  sprintf(
    "phys_contact_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  phys_contact_summary,
  title = "Physical contact",
  subtitle = "Proportion of respondents answering 'In the last week, have you had physical\ncontact with a person who doesn’t live with you? (e.g. shaken hands, hugged,\nbody contact during sports)'"
)

save_ggplot(
  sprintf(
    "phys_contact_line_%s.png",
    save_date
  )
)

# wash_hands - q110
# Thinking about the last time you were in a public place, did you wash your hands or use hand sanitiser immediately afterwards?
wash_hands_summary <- single_reponse_summary(all_survey_results, wash_hands) %>%
  mutate(
    response = case_when(
      response == "Yes, I washed or sanitised my hands immediately afterward" ~ "Yes, immediately",
      response == "I washed or sanitised my hands as soon as I could, but there was a delay" ~ "Yes, delayed",
      response == "No" ~ "No"
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  wash_hands_summary,
  title = "Hand washing behaviour",
  subtitle = "Proportion of respondents answering 'Thinking about the last time you\n were in a public place, did you wash your hands or use hand sanitiser\nimmediately afterwards?'"
)

save_ggplot(
  sprintf(
    "wash_hands_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  wash_hands_summary,
  title = "Hand washing behaviour",
  subtitle = "Proportion of respondents answering 'Thinking about the last time you\n were in a public place, did you wash your hands or use hand sanitiser\nimmediately afterwards?'"
)

save_ggplot(
  sprintf(
    "wash_hands_line_%s.png",
    save_date
  )
)

# cough - q111
# Thinking about the last time you coughed or sneezed, did you cover your mouth with…
cough_summary <- single_reponse_summary(all_survey_results, cough) %>%
  mutate(
    response = case_when(
      response == "Something else (e.g. a hanky, clothing)" ~ "Something else",
      TRUE ~ response
    ) %>% 
      factor(
        levels = c("Nothing", "Your hand", "Your elbow", "A tissue", "Something else")
      )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  cough_summary,
  title = "Coughing behaviour",
  subtitle = "Proportion of respondents answering 'Thinking about the last time you\ncoughed or sneezed, did you cover your mouth with…'"
)

save_ggplot(
  sprintf(
    "cough_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  cough_summary,
  title = "Coughing behaviour",
  subtitle = "Proportion of respondents answering 'Thinking about the last time you\ncoughed or sneezed, did you cover your mouth with…'"
)

save_ggplot(
  sprintf(
    "cough_line_%s.png",
    save_date
  )
)

# face_covering - q222
face_covering_summary <- single_reponse_summary(all_survey_results, face_covering) %>%
  mutate(
    response = factor(
      response,
      levels = c("No", "Rarely", "Sometimes", "Often", "Always")
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  face_covering_summary,
  title = "Mask wearing behaviour",
  subtitle = "Proportion of respondents answering'Do you wear a face covering whenever\nyou leave your home? A face covering needs to cover both your nose and\nmouth. It could be a face mask or shield'"
)

save_ggplot(
  sprintf(
    "face_covering_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  face_covering_summary,
  title = "Mask wearing behaviour",
  subtitle = "Proportion of respondents answering'Do you wear a face covering whenever\nyou leave your home? A face covering needs to cover both your nose and\nmouth. It could be a face mask or shield'"
)

save_ggplot(
  sprintf(
    "face_covering_line_%s.png",
    save_date
  )
)

# how_likely_to_catch - q14
# How likely do you think it is that you will catch COVID-19?
how_likely_to_catch_summary <- single_reponse_summary(all_survey_results, how_likely_to_catch) %>%
  mutate(
    response = factor(
      response,
      levels = c("Very unlikely", "Unlikely", "Neutral", "Likely", "Very likely")
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  how_likely_to_catch_summary,
  title = "Expectation of infection",
  subtitle = "Proportion of respondents answering 'How likely do you think it is that you\nwill catch COVID-19?'"
)

save_ggplot(
  sprintf(
    "how_likely_to_catch_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  how_likely_to_catch_summary,
  title = "Expectation of infection",
  subtitle = "Proportion of respondents answering 'How likely do you think it is that you\nwill catch COVID-19?'"
)

save_ggplot(
  sprintf(
    "how_likely_to_catch_line_%s.png",
    save_date
  )
)

# attitude_severe - q56
# If you were to get Coronavirus, how severely do you think it would affect you? 
attitude_severe_summary <- single_reponse_summary(all_survey_results, attitude_severe) %>%
  mutate(
    response = case_when(
      response == "I don't know" ~ "Unknown",
      response == "I don^t know" ~ "Unknown",
      response == "I dont know" ~ "Unknown",
      response == "Not at all" ~ "None",
      response == "The symptoms would be mild, as in a cold" ~ "Mild",
      response == "The symptoms would be pretty bad, as in a flu" ~ "Moderate",
      response == "The symptoms would be severe, and I may need to be hospitalised" ~ "Severe",
      response == "The virus may kill me" ~ "Death"
    )
  ) %>%
  group_by(state, wave_date, response) %>%
  summarise(
    count = sum(count),
    proportion = sum(proportion)
  ) %>%
  mutate(
    response = factor(
      response,
      levels = c(
        "None",
        "Mild",
        "Moderate",
        "Severe",
        "Death",
        "Unknown"
      )
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  attitude_severe_summary,
  title = "Attitudes to severity of COVID-19",
  subtitle = "Proportion of respondents answering 'If you were to get Coronavirus, how severely\ndo you think it would affect you?'"
)


save_ggplot(
  sprintf(
    "attitude_severe_stack_%s.png",
    save_date
  )
)


plot_single_response_line(
  attitude_severe_summary,
  title = "Attitudes to severity of COVID-19",
  subtitle = "Proportion of respondents answering 'If you were to get Coronavirus, how severely\ndo you think it would affect you?'"
)


save_ggplot(
  sprintf(
    "attitude_severe_line_%s.png",
    save_date
  )
)

# vaccinated - q224
# Have you received a COVID-19 vaccination?

vaccinated_summary <- single_reponse_summary(all_survey_results, vaccinated) %>%
  mutate(
    response = case_when(
      str_ends(response, "first dose") ~ "First dose",
      str_ends(response, "both doses") ~ "Second dose",
      str_ends(response, "booster dose") ~ "Third dose",
      response == "No" ~ "No"
    ) %>%
      factor(
        levels = c("No", "First dose", "Second dose", "Third dose")
      )
  ) %>%
  group_by(state, wave_date, response) %>%
  summarise(
    count = sum(count),
    proportion = sum(proportion),
    .groups = "drop"
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  vaccinated_summary,
  title = "Vaccination",
  subtitle = "Proportion of respondents answering 'Have you received a COVID-19 vaccination?'"
)

save_ggplot(
  sprintf(
    "vaccinated_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  vaccinated_summary,
  title = "Vaccination",
  subtitle = "Proportion of respondents answering 'Have you received a COVID-19 vaccination?'"
)

save_ggplot(
  sprintf(
    "vaccinated_line_%s.png",
    save_date
  )
)

# informed_contact - q233
# Have you been informed (by a Health department, workplace, colleague, friend or through any other means) that you were in contact with a confirmed COVID-19 case in the past week?  
informed_contact_summary <- single_reponse_summary(all_survey_results, informed_contact) %>%
  mutate(
    response = factor(
      response,
      levels = c("Prefer not to say", "No", "Yes")
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  informed_contact_summary,
  title = "Contact with cases",
  subtitle = "Proportion of respondents answering 'Have you been informed (by a Health\ndepartment, workplace, colleague, friend or through any other means) that\nyou were in contact with a confirmed COVID-19 case in the past week?'",
  date_breaks = "2 months"
)

save_ggplot(
  sprintf(
    "informed_contact_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  informed_contact_summary,
  title = "Contact with cases",
  subtitle = "Proportion of respondents answering 'Have you been informed (by a Health\ndepartment, workplace, colleague, friend or through any other means) that\nyou were in contact with a confirmed COVID-19 case in the past week?'",
  date_breaks = "2 months"
)

save_ggplot(
  sprintf(
    "informed_contact_line_%s.png",
    save_date
  )
)

# response_to_informed_contact - Q234 (this is dependant on response 1 in informed_contact, so should be otherwise NA can be analysed as a single response question 
response_to_informed_contact_summary <- single_reponse_summary(all_survey_results, response_to_informed_contact) %>%
  mutate(
    response = case_when(
      response == "I am completely isolating – not leaving home for any reason" ~ "Completely\nisolating",
      response == "I am not trying to stay home" ~ "Not staying\nhome",
      response == "I am staying home as much as I can, and only going out when I absolutely have to" ~ "Mostly\nisolating",
      response == "I am still choosing to leave my home on a regular basis, but I am going out less than usual" ~ "Staying home\na little"
    )
  ) %>%
  mutate(
    response = factor(
      response,
      levels = c(
        "Not staying\nhome",
        "Staying home\na little",
        "Mostly\nisolating",
        "Completely\nisolating"
      )
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  response_to_informed_contact_summary,
  title = "Response to contact with cases",
  subtitle = "Proportion of respondents answering 'Which of the following best describes\nthe approach you have been taking in the last week as a result of being in\ncontact with a confirmed COVID-19 case?'",
  date_breaks = "2 months"
)

save_ggplot(
  sprintf(
    "response_to_informed_contact_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  response_to_informed_contact_summary,
  title = "Response to contact with cases",
  subtitle = "Proportion of respondents answering 'Which of the following best describes\nthe approach you have been taking in the last week as a result of being in\ncontact with a confirmed COVID-19 case?'",
  date_breaks = "2 months"
)

save_ggplot(
  sprintf(
    "response_to_informed_contact_line_%s.png",
    save_date
  )
)


# Multiple responses


# dependent responses
# consider looking at #attitude_severe | how_likely_to_catch
# guidelines_given_vaccinated - q225b | does level of vaccination effect willingness to follow guidelines


single_reponse_summary <- function(data, var){
  
  data %>%
    select(
      #wave,
      wave_date,
      state,
      response = {{var}}
    ) %>% 
    filter(
      !is.na(response),
      !is.na(state)
    )  %>%
    group_by(state, wave_date, response) %>%
    summarise(count = n()) %>%
    ungroup %>%
    complete(state, wave_date, response, fill = list(count = 0)) %>%
    arrange(state, wave_date, response) %>%
    group_by(state, wave_date) %>%
    mutate(
      n = sum(count),
      proportion = count/n
    ) %>%
    ungroup %>%
    select(-n)
  
}




### template


XXX_summary <- single_reponse_summary(all_survey_results, XXX) %>%
  mutate(
    response = factor(
      response,
      levels = c("No", "Rarely", "Sometimes", "Often", "Always")
    )
  ) %>%
  arrange(state, wave_date, response)


plot_single_response_stack(
  XXX_summary,
  title = "XX behaviour",
  subtitle = "Proportion of respondents answering\n'XX?'"
)

save_ggplot(
  sprintf(
    "XXX_stack_%s.png",
    save_date
  )
)

plot_single_response_line(
  XXX_summary,
  title = "XX behaviour",
  subtitle = "Proportion of respondents answering\n'XX?'"
)

save_ggplot(
  sprintf(
    "XXX_%s.png",
    save_date
  )
)


