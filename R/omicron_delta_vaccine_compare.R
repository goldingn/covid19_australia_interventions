require(tidyverse)
require(lubridate)
green <- RColorBrewer::brewer.pal(8, "Set2")[1]

# ------------------------ #

the.date <- "19Jan2022"
vacc.start <- ymd("2021-02-22")

omicron_no_vax <- read_csv(paste0("outputs/projection/omicron/r_eff_1_local_samples.csv"),
                  col_types =cols(
                    .default = col_double(),
                    date = col_date(format = ""),
                    state = col_character(),
                    date_onset = col_date(format = "")
                  ))



omicron_vax <- read_csv(paste0("outputs/projection/omicron_vax/r_eff_1_local_samples.csv"),
                  col_types =cols(
                    .default = col_double(),
                    date = col_date(format = ""),
                    state = col_character(),
                    date_onset = col_date(format = "")
                  )) 

delta_no_vax <- read_csv(paste0("outputs/projection/delta/r_eff_1_local_samples.csv"),
                           col_types =cols(
                             .default = col_double(),
                             date = col_date(format = ""),
                             state = col_character(),
                             date_onset = col_date(format = "")
                           ))



delta_vax <- read_csv(paste0("outputs/projection/delta_vax/r_eff_1_local_samples.csv"),
                        col_types =cols(
                          .default = col_double(),
                          date = col_date(format = ""),
                          state = col_character(),
                          date_onset = col_date(format = "")
                        )) 

start.date <- ymd("2021-02-01")
end.date <- ymd("2022-01-19")
date.label.format <- "%b"
n.week.labels.panel <- 1
n.week.ticks <- "1 months"

# Create date objects for ticks/labels (e.g., show ticks every n.week.ticks, but label every n.week.labels.panel)
dd <- format(seq.Date(ymd(start.date), end.date, by=n.week.ticks), date.label.format)
dd.labs <- as.character(dd)
dd.labs[!(dd.labs %in% dd.labs[(seq(length(dd.labs),1,by=-n.week.labels.panel))])] <- ""
dd.labs <- gsub(pattern = "^0", replacement = "", x = dd.labs)
dd.labs <- gsub(pattern = "/0", replacement = "/", x = dd.labs)
dd <- seq.Date(ymd(start.date), end.date, by=n.week.ticks)

# Quantiles
qs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

r1 <- delta_vax %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95") %>% filter(date <= end.date)


r2 <- omicron_vax %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95")  %>% filter(date <= end.date)

r2.post <- r2 %>% filter(date >= vacc.start)


# Plot aesthetics
outer.alpha <- 0.15
inner.alpha <- 0.4
line.alpha <- 0.8
col1 <- "grey70"
col2 <- green


subset.states <- c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA")
# subset.states <- c("NSW")
## Figure showing Reff VOC v non-VOC by state
ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions() %>% filter(state %in% subset.states),
    colour = "grey80"
  ) +
  
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col1, alpha=outer.alpha) +
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col1, alpha=inner.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col1, alpha = line.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col1, alpha = line.alpha) +
  
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col2, alpha=outer.alpha) +
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col2, alpha=inner.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col2, alpha = line.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col2, alpha = line.alpha) +
  
  geom_vline(
    data = prop_variant_dates() %>% filter(state %in% subset.states),
    aes(xintercept = date),
    colour = "firebrick1",
    linetype = 5
  ) +
  
  geom_vline(xintercept = vacc.start, colour = "steelblue3", linetype = 5) +
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  scale_y_continuous("", position = "right", breaks = seq(0,5,by=1)) +
  scale_x_date("Date", breaks = dd, labels = dd.labs) +
  # scale_x_date("Date", date_breaks = "1 month", date_labels = "%e/%m") +
  
  coord_cartesian(xlim = c(start.date, end.date),
                  ylim = c(0, 5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(size = 9),
        panel.spacing = unit(1.2, "lines"))

ggsave(paste0("outputs/figures/omicron_vs_delta_vacc_",the.date,".png"), height = 10, width = 9, bg = "white")







# omicron_vac_no_vac ------------------------------------------------------




r1 <- omicron_no_vax %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95") %>% filter(date <= end.date)


r2 <- omicron_vax %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95")  %>% filter(date <= end.date)

r2.post <- r2 %>% filter(date >= vacc.start)


# Plot aesthetics
outer.alpha <- 0.15
inner.alpha <- 0.4
line.alpha <- 0.8
col1 <- "grey70"
col2 <- green


subset.states <- c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA")
# subset.states <- c("NSW")
## Figure showing Reff VOC v non-VOC by state
ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions() %>% filter(state %in% subset.states),
    colour = "grey80"
  ) +
  
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col1, alpha=outer.alpha) +
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col1, alpha=inner.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col1, alpha = line.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col1, alpha = line.alpha) +
  
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col2, alpha=outer.alpha) +
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col2, alpha=inner.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col2, alpha = line.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col2, alpha = line.alpha) +
  
  geom_vline(
    data = prop_variant_dates() %>% filter(state %in% subset.states),
    aes(xintercept = date),
    colour = "firebrick1",
    linetype = 5
  ) +
  
  geom_vline(xintercept = vacc.start, colour = "steelblue3", linetype = 5) +
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  scale_y_continuous("", position = "right", breaks = seq(0,5,by=1)) +
  scale_x_date("Date", breaks = dd, labels = dd.labs) +
  # scale_x_date("Date", date_breaks = "1 month", date_labels = "%e/%m") +
  
  coord_cartesian(xlim = c(start.date, end.date),
                  ylim = c(0, 5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(size = 9),
        panel.spacing = unit(1.2, "lines"))

ggsave(paste0("outputs/figures/omicron_novacc_vacc_",the.date,".png"), height = 10, width = 9, bg = "white")


# delta vac no vac --------------------------------------------------------


r1 <- delta_no_vax %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95") %>% filter(date <= end.date)


r2 <- delta_vax %>% 
  reshape2::melt(id.vars = c("date","state","date_onset")) %>%
  group_by(date,state) %>% 
  summarise(x = quantile(value, qs), q = qs) %>% 
  reshape2::dcast(state+date ~ q, value.var = "x") %>%
  rename("L90"="0.05", "L50"="0.25", "med"="0.5", "U50"="0.75", "U90"="0.95")  %>% filter(date <= end.date)

r2.post <- r2 %>% filter(date >= vacc.start)


# Plot aesthetics
outer.alpha <- 0.15
inner.alpha <- 0.4
line.alpha <- 0.8
col1 <- "grey70"
col2 <- green


subset.states <- c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA")
# subset.states <- c("NSW")
## Figure showing Reff VOC v non-VOC by state
ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  
  geom_vline(
    aes(xintercept = date),
    data = interventions() %>% filter(state %in% subset.states),
    colour = "grey80"
  ) +
  
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col1, alpha=outer.alpha) +
  geom_ribbon(data = r1 %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col1, alpha=inner.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col1, alpha = line.alpha) +
  geom_line(data = r1 %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col1, alpha = line.alpha) +
  
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L90, ymax = U90), fill = col2, alpha=outer.alpha) +
  geom_ribbon(data = r2.post %>% filter(state %in% subset.states), aes(x = date, ymin = L50, ymax = U50), fill = col2, alpha=inner.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = L90), col = col2, alpha = line.alpha) +
  geom_line(data = r2.post %>% filter(state %in% subset.states), aes(x = date, y = U90), col = col2, alpha = line.alpha) +
  
  geom_vline(
    data = prop_variant_dates() %>% filter(state %in% subset.states),
    aes(xintercept = date),
    colour = "firebrick1",
    linetype = 5
  ) +
  
  geom_vline(xintercept = vacc.start, colour = "steelblue3", linetype = 5) +
  
  facet_wrap(~state, ncol = 2, scales = "free") +
  
  scale_y_continuous("", position = "right", breaks = seq(0,5,by=1)) +
  scale_x_date("Date", breaks = dd, labels = dd.labs) +
  # scale_x_date("Date", date_breaks = "1 month", date_labels = "%e/%m") +
  
  coord_cartesian(xlim = c(start.date, end.date),
                  ylim = c(0, 5)) +
  
  cowplot::theme_cowplot() +
  cowplot::panel_border(remove = TRUE) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.title.y.right = element_text(vjust = 0.5, angle = 90),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(size = 9),
        panel.spacing = unit(1.2, "lines"))

ggsave(paste0("outputs/figures/delta_novacc_vacc_",the.date,".png"), height = 10, width = 9, bg = "white")
