
proj_samples <- read.csv("outputs/projection/r_eff_12_local_samples.csv") %>%
  as_tibble %>%
  pivot_longer(
    cols = starts_with("sim"),
    values_to = "value",
    names_to = "name"
  )


today_samples <- proj_samples %>%
  filter(state == "VIC", date == "2021-08-23")

mean(today_samples$value)


quantile(today_samples$value, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

proj_samples %>%
  filter(state == "VIC", date == "2021-08-09") %>%
  pull(value) %>%
  quantile(probs = c(0.05, 0.25, 0.5, 0.75, 0.95))



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

c12_samples <- read_reff_samples("outputs/projection/r_eff_12_local_samples.csv")

ggplot(c12) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75), alpha = 0.5) +
  geom_line(aes(x = date, y = med), alpha = 0.5) +
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



c2_samples <- read_reff_samples("outputs/projection/r_eff_2_samples.csv")

ggplot(c2_samples) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75), alpha = 0.25) +
  geom_line(aes(x = date, y = med), alpha = 0.25) +
  facet_wrap(~state) 


c1_old <- read_reff_samples("outputs/projection_oldmatrix/r_eff_1_local_samples.csv") %>%
  mutate(matrix = "old")
c1_new <- read_reff_samples("outputs/projection/r_eff_1_local_samples.csv") %>%
  mutate(matrix = "new")

c12_old <- read_reff_samples("outputs/projection_oldmatrix/r_eff_12_local_samples.csv") %>%
  mutate(matrix = "old")
c12_new <- read_reff_samples("outputs/projection/r_eff_12_local_samples.csv") %>%
  mutate(matrix = "new")


c1 <- bind_rows(c1_old, c1_new)
c12 <- bind_rows(c12_old, c12_new)

ggplot(c1) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = matrix), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = matrix), alpha = 0.25) +
  geom_line(aes(x = date, y = med, linetype = matrix), alpha = 0.25) +
  facet_wrap(~state, ncol = 2) +
  scale_fill_manual(values = c(green, "grey80")) +
  scale_colour_manual(values = c(green, "grey80")) +
  theme_cowplot() +
  geom_vline(xintercept = as.Date("2021-02-22"), col = "steelblue") +
  scale_y_continuous(limits = c(0, 5))

ggplot(c12) +
  geom_ribbon(aes(x = date, ymin = lw5, ymax = up95, col = matrix), alpha = 0) +
  geom_ribbon(aes(x = date, ymin = lw25, ymax = up75, fill = matrix), alpha = 0.25) +
  geom_line(aes(x = date, y = med, linetype = matrix), alpha = 0.25) +
  facet_wrap(~state, ncol = 2) +
  scale_fill_manual(values = c(green, "grey80")) +
  scale_colour_manual(values = c(green, "grey80")) +
  theme_cowplot() +
  geom_vline(xintercept = as.Date("2021-02-22"), col = "steelblue") +
  scale_y_continuous(limits = c(0, 5))
  

vthist <- read_csv("outputs/vaccine_effect_timeseries_2021-11-07.csv") %>%
  mutate(method = "Historic")
vtmat <- read_csv("outputs/vaccine_effect_timeseries_2021-11-14.csv") %>%
  mutate(method = "Contact\nmatrix\nupdate")
vtdata <- readRDS("outputs/vaccine_effect_timeseries.RDS") %>%
  mutate(method = "Data\nupdate")

vt <- bind_rows(vthist, vtmat, vtdata) %>%
  mutate(
    method = method %>%
      factor(
        levels = c(
          "Data\nupdate",
          "Contact\nmatrix\nupdate",
          "Historic"
        )
      )
  )


ggplot(vt) +
  geom_line(
    aes(
      x = date,
      y = effect,
      #colour = state,
      #alpha = method
      colour = method
    ),
    size = 1.5,
    alpha = 0.7
  ) +
  theme_classic() +
  labs(
    x = NULL,
    y = "Change in transmission potential",
    #col = "State",
    #alpha = "Method"
    col = "Method"
  ) +
  scale_x_date(
    breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  ggtitle(
    label = "Vaccination effect",
    subtitle = "Change in transmission potential due to vaccination"
  ) +
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(
    strip.background = element_blank(),
    axis.title.y.right = element_text(vjust = 0.5, angle = 90, size = font_size),
    #legend.position = c(0.02, 0.135),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 270),
    plot.title = element_text(size = font_size + 8),
    plot.subtitle = element_text(size = font_size)
  ) +
  # scale_colour_manual(
  #   values = c(
  #     "darkgray",
  #     "cornflowerblue",
  #     "chocolate1",
  #     "violetred4",
  #     "red1",
  #     "darkgreen",
  #     "darkblue",
  #     "gold1"
  #   )
  # ) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  ) +
  #scale_alpha_manual(values = c(0.5, 1, 0.2)) +
  #scale_linetype_manual(values = c(1, 2, 3)) +
  facet_wrap(~state, ncol = 2)

ggsave(
  filename = "outputs/figures/vaccination_effect_comparison_all.png",
  dpi = dpi,
  width = 1500 / dpi,
  height = 1250 / dpi,
  scale = 1.2
)
