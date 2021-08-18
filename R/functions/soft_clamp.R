# given a dataframe of Reff trajectory samples for Rob M, 'soft-clamp' the Reff
# trajectories so that the log-mean of Reff is constant into the future from the target date, but the
# trajectories still vary over time (rather than hard-clamping them so the
# trajectory for each Reff is flat into the future).
soft_clamp <- function(local_samples, target_date) {
  local_samples %>%
    pivot_longer(
      cols = starts_with("sim"),
      names_to = "sim",
      values_to = "reff"
    ) %>%
    group_by(
      date,
      state
    ) %>%
    mutate(
      log_reff = log(reff),
      log_mean = mean(log_reff)
    ) %>%
    group_by(
      state
    ) %>%
    mutate(
      latest_log_mean = mean(log_reff[date == target_date]),
      adjust = ifelse(date > target_date, latest_log_mean - log_mean, 0),
      log_reff = log_reff + adjust,
      reff = exp(log_reff)
    ) %>%
    ungroup() %>%
    select(-log_reff, -log_mean, -latest_log_mean, -adjust) %>%
    pivot_wider(
      names_from = sim,
      values_from = reff
    ) 
}
