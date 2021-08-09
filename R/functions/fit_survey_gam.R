fit_survey_gam <- function(
  fit_dat,
  pred_dat
){
  
  respondents <- fit_dat$respondents
  count <- fit_dat$count
  date <- fit_dat$date
  intervention_stage <- fit_dat$intervention_stage
  
  date_num <- as.numeric(date - min(date))
  
  m <- mgcv::gam(
    cbind(count, I(respondents - count)) ~ s(date_num) + intervention_stage,
    select = TRUE,
    family = stats::binomial,
  )
  
  
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
  
  fitted <- m$family$linkinv(pred$fit) * pred_dat$distancing
  ci_90_hi <- m$family$linkinv(ci_90_hi) * pred_dat$distancing
  ci_90_lo <- m$family$linkinv(ci_90_lo) * pred_dat$distancing
  ci_50_hi <- m$family$linkinv(ci_50_hi) * pred_dat$distancing
  ci_50_lo <- m$family$linkinv(ci_50_lo) * pred_dat$distancing
  
  
  
  tibble(
    date = pred_dat$date,
    mean = fitted ,
    ci_90_lo,
    ci_50_lo,
    ci_50_hi,
    ci_90_hi
  )
  
}
