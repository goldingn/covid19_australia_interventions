local_cases <- tibble::tibble(
  date_onset = rep(data$dates$onset, data$n_states),
  detection_probability = as.vector(data$detection_prob_mat),
  state = rep(data$states, each = data$n_dates),
  count = as.vector(data$local$cases_infectious),
  acquired_in_state = as.vector(data$local$cases)
) 



lc_long <- local_cases %>%
  filter(date_onset >"2021-11-30") %>%
  filter(detection_probability > 0.01) %>%
  select(-acquired_in_state) %>%
  mutate(projected_count = count/detection_probability) %>%
  group_by(state, date_onset) %>%
  mutate(proj = projected_count - count) %>%
  select(-projected_count) %>%
  pivot_longer(cols = c("count", "proj"), names_to = "type", values_to = "count")

prob_line_95 <- lc_long %>%
  filter(type == "count") %>%
  filter(detection_probability >= 0.95) %>%
  group_by(state) %>%
  filter(detection_probability == min(detection_probability)) %>%
  select(state,date_onset)

prob_line_90 <- lc_long %>%
  filter(type == "count") %>%
  filter(detection_probability >= 0.9) %>%
  group_by(state) %>%
  filter(detection_probability == min(detection_probability)) %>%
  select(state,date_onset)

lc_long %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_onset,
      y = count,
      fill = type
    ),
    stat = "identity"
  ) +
  geom_vline(
    data = prob_line_95,
    aes(
      xintercept = date_onset
    )
  ) +
  geom_vline(
    data = prob_line_90,
    aes(
      xintercept = date_onset
    )
  ) +
  facet_wrap(
    facets = vars(state),
    ncol = 2,
    scales = "free_y"
  )

ggsave("outputs/figures/watermelon.png", bg = 'white')