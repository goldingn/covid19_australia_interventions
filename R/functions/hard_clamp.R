# given a dataframe of Reff trajectory samples for Rob M, 'hard-clamp' the Reff
# trajectories so that the trajectory for each Reff is costant into the future
# from the target date.
hard_clamp <- function(local_samples, target_date) {
  local_samples %>%
    pivot_longer(
      cols = starts_with("sim"),
      names_to = "sim",
      values_to = "reff"
    ) %>%
    group_by(
      state,
      sim
    ) %>%
    mutate(
      target_reff = reff[date == target_date],
      reff = ifelse(date > target_date, reff[date == target_date], reff),
    ) %>%
    ungroup() %>%
    select(-target_reff) %>%
    pivot_wider(
      names_from = sim,
      values_from = reff
    )
}
