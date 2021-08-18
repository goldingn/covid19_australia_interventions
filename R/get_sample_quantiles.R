
proj_samples <- read.csv("outputs/projection/r_eff_12_local_samples.csv") %>%
  as_tibble %>%
  pivot_longer(
    cols = starts_with("sim"),
    values_to = "value",
    names_to = "name"
  )


today_samples <- proj_samples %>%
  filter(state == "VIC", date == "2021-05-31")

mean(today_samples$value)


quantile(today_samples$value, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))


vic_samples <- proj_samples %>%
  filter(state == "VIC") %>%
  group_by(date) %>%
  summarise(mean = mean(value)) %>%
  mutate(date = as.Date(as.character(date)))




recent_samples <- vic_samples %>%
  filter(date > "2021-05-01", date < "2021-05-30")


peak <- recent_sampes[which.max(recent_samples$mean),]



peak_samples <- proj_samples %>%
  filter(state == "VIC", date == "2021-05-12")

mean(peak_samples$value)


quantile(peak_samples$value, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))


pwt_samples <- read.csv("outputs/proj_wt/r_eff_12_local_samples.csv") %>%
  as_tibble %>%
  pivot_longer(
    cols = starts_with("sim"),
    values_to = "value",
    names_to = "name"
  )

alpha12 <- proj_samples %>%
  group_by(date, state) %>%
  summarise(
    med = median(value),
    lw5 = quantile(value, 0.05),
    up95 = quantile(value, 0.95),
    lw25 = quantile(value, 0.25),
    up75 = quantile(value, 0.75)
  )

wt12 <- pwt_samples %>%
  group_by(date, state) %>%
  summarise(
    med = median(value),
    lw5 = quantile(value, 0.05),
    up95 = quantile(value, 0.95),
    lw25 = quantile(value, 0.25),
    up75 = quantile(value, 0.75)
  )


c12 <- bind_rows(alpha12 %>% mutate(variant = "alpha"), wt12 %>% mutate(variant = "wt")) %>% ungroup %>%
  mutate(date = as.Date(as.character(date)))


ggplot(c12 %>% filter(date < "2021-07-05")) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = variant), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = variant), alpha = 0.5) +
  geom_line(aes(x = date, y = med, linetype = variant), alpha = 0.5) +
  facet_wrap(~state) 

sample.file <- "outputs/projection/r_eff_1_local_samples.csv"

read_reff_samples <- function(
  sample.file
){
  
  read.csv(sample.file) %>%
    as_tibble %>%
    pivot_longer(
      cols = starts_with("sim"),
      values_to = "value",
      names_to = "name"
    )  %>%
    group_by(date, state) %>%
    summarise(
      med = median(value),
      lw5 = quantile(value, 0.05),
      up95 = quantile(value, 0.95),
      lw25 = quantile(value, 0.25),
      up75 = quantile(value, 0.75)
    ) %>%
    ungroup %>%
    mutate(date = as.Date(as.character(date)))
}

mod_samples <- read_reff_samples("outputs/projection/r_eff_1_local_samples.csv")
wt_samples <- read_reff_samples("outputs/projection/wt/r_eff_1_local_samples.csv")
alpha_samples <- read_reff_samples("outputs/projection/alpha/r_eff_1_local_samples.csv")
delta_samples <- read_reff_samples("outputs/projection/delta/r_eff_1_local_samples.csv")



c1 <- bind_rows(
  mod_samples %>%
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


ttiq_samples <- read_reff_samples("outputs/projection/r_eff_1_local_samples.csv")
mod_samples <- read_reff_samples("outputs/projection/standard/r_eff_1_local_samples.csv")



c1 <- bind_rows(
  mod_samples %>%
    mutate(variant = "model"),
  ttiq_samples %>%
    mutate(variant = "ttiq")
)

#%>% filter(date < "2021-07-05")

ggplot(c1) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = variant), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = variant), alpha = 0.5) +
  geom_line(aes(x = date, y = med, linetype = variant), alpha = 0.5) +
  facet_wrap(~state) 


ttiq12_samples <- read_reff_samples("outputs/projection/r_eff_12_local_samples.csv")
mod12_samples <- read_reff_samples("outputs/projection/standard/r_eff_12_local_samples.csv")



c12 <- bind_rows(
  mod12_samples %>%
    mutate(variant = "model"),
  ttiq12_samples %>%
    mutate(variant = "ttiq")
)

#%>% filter(date < "2021-07-05")

ggplot(c12) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = variant), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = variant), alpha = 0.5) +
  geom_line(aes(x = date, y = med, linetype = variant), alpha = 0.5) +
  facet_wrap(~state) 



vax_samples <- read_reff_samples("outputs/projection/r_eff_1_local_with_vaccine_samples.csv")
mod_samples <- read_reff_samples("outputs/projection/r_eff_1_local_samples.csv")



c1 <- bind_rows(
  mod_samples %>%
    mutate(vaccination = "no"),
  vax_samples %>%
    mutate(vaccination = "yes")
)

#%>% filter(date < "2021-07-05")

ggplot(c1) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = vaccination), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = vaccination), alpha = 0.25) +
  geom_line(aes(x = date, y = med, linetype = vaccination), alpha = 0.25) +
  facet_wrap(~state) 

