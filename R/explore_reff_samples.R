

model_samples <- read_reff_samples("outputs/projection/r_eff_1_local_samples.csv")
wt_samples    <- read_reff_samples("outputs/projection/wt/r_eff_1_local_samples.csv")
alpha_samples <- read_reff_samples("outputs/projection/alpha/r_eff_1_local_samples.csv")
delta_samples <- read_reff_samples("outputs/projection/delta/r_eff_1_local_samples.csv")

c1 <- bind_rows(
  model_samples %>%
    mutate(variant = "model"),
  wt_samples %>%
    mutate(variant = "wt"),
  alpha_samples %>%
    mutate(variant = "alpha"),
  delta_samples %>%
    mutate(variant = "delta")
)

#%>% filter(date < "2021-07-05")

ggplot(c1) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = variant), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = variant), alpha = 0.5) +
  geom_line(aes(x = date, y = med, linetype = variant), alpha = 0.5) +
  facet_wrap(~state) 




delta <- read_reff_samples("outputs/projection/delta/r_eff_1_local_samples.csv")
delta_vax <- read_reff_samples("outputs/projection/delta_vax/r_eff_1_local_samples.csv")
omicron <- read_reff_samples("outputs/projection/omicron/r_eff_1_local_samples.csv")
omicron_vax <- read_reff_samples("outputs/projection/omicron_vax/r_eff_1_local_samples.csv")

c1 <- bind_rows(
  delta %>%
    mutate(
      variant = "delta",
      vaccine_effect = "no_vax"
    ),
  delta_vax %>%
    mutate(
      variant = "delta",
      vaccine_effect = "vax"
    ),
  omicron %>%
    mutate(
      variant = "omicron",
      vaccine_effect = "no_vax"
    ),
  omicron_vax %>%
    mutate(
      variant = "omicron",
      vaccine_effect = "vax"
    )
) %>%
  filter(
    date >= "2021-02-22"
  )

#%>% filter(date < "2021-07-05")

ggplot(c1 %>% filter(vaccine_effect == "no_vax")) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = variant), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = variant), alpha = 0.5) +
  geom_line(aes(x = date, y = med, linetype = variant), alpha = 0.5) +
  facet_wrap(~state) 


ggplot(c1 %>% filter(vaccine_effect == "vax")) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = variant), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = variant), alpha = 0.5) +
  geom_line(aes(x = date, y = med, linetype = variant), alpha = 0.5) +
  facet_wrap(~state) 

