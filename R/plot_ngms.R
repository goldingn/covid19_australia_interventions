# plot national NGM

source("R/functions.R")
library(conmat)
library(tidyverse)

# define age breaks for prediction
age_breaks <- c(seq(0, 80, by = 5), Inf)


# fit polymod model
setting_models <- fit_setting_contacts(
  contact_data_list = get_polymod_setting_data(),
  population = get_polymod_population()
)

australia_pop <- abs_pop_age_lga_2020 %>%
  group_by(age_group) %>%
  summarise(
    population = sum(population)
  ) %>%
  mutate(
    lower.age.limit = readr::parse_number(as.character(age_group))
  ) %>%
  mutate(
    country = "Australia"
  )

# get Australia-wide setting-specific contact matrices
# with adjustment to the household size
australia_setting_contact_matrices_5y <- predict_setting_contacts(
  contact_model = setting_models,
  population = australia_pop,
  per_capita_household_size = get_per_capita_household_size(),
  age_breaks = age_breaks
)

# get the setting-specific contact matrices
setting_transmission_matrices_5y <- get_setting_transmission_matrices(
  age_breaks = age_breaks,
  asymptomatic_relative_infectiousness = 0.5,
  susceptibility_estimate = "davies_updated"
)

settings <- c("home", "school", "work", "other")

# setting specific unscaled NGMs
australia_setting_ngms_5y <- mapply(
  "*",
  australia_setting_contact_matrices_5y[settings],
  setting_transmission_matrices_5y[settings],
  SIMPLIFY = FALSE
)

# overall NGM
australia_ngm_5y <- Reduce("+", australia_setting_ngms_5y)

# calibrate to TP = 3.6
m <- find_m(3.6, australia_ngm_5y)
new_matrix <- australia_ngm_5y * m

# and old matrix for comparison
old_matrix <- baseline_matrix(3.6)

# inf <- 1:17
# mat <- matrix(1, 17, 17)
# mat2 <- sweep(mat, 2, inf, FUN = "*")
# mat2 %*% rep(1, 17)

# setting_transmission_matrices_5y$home




age_names <- age_classes()$classes


old_matrix_df <- old_matrix %>%
  `colnames<-`(age_names) %>%
  as_tibble() %>%
  mutate(
    infectees = age_names
  ) %>%
  pivot_longer(
    cols = -infectees, 
    names_to = "infectors",
    values_to = "TP\ncontribution"
  ) %>%
  mutate(
    matrix = "previous"
  )

new_matrix_df <- new_matrix %>%
  `colnames<-`(age_names) %>%
  as_tibble() %>%
  mutate(
    infectees = age_names
  ) %>%
  pivot_longer(
    cols = -infectees, 
    names_to = "infectors",
    values_to = "TP\ncontribution"
  ) %>%
  mutate(
    matrix = "updated"
  )

# plot as in phase 1 report
bind_rows(
  old_matrix_df, new_matrix_df
) %>%
  mutate(
    across(
      c(infectors, infectees),
      ~factor(., levels = age_names)
    ),
    matrix = factor(
      matrix,
      levels = c("previous", "updated"),
      labels = c("Previous matrix", "Updated matrix")
    )
  ) %>%
  ggplot(
    aes(
      x = infectors,
      y = infectees,
      fill = `TP\ncontribution`
    )
  ) +
  coord_fixed() +
  geom_tile() +
  scale_fill_distiller(
    direction = 1
  ) +
  facet_wrap(~matrix) +
  theme_minimal() +
  theme(
    axis.text = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(
      size = 12
    )
  )

ggsave(
  "outputs/reopening/transmission_matrices.png",
  width = 10,
  height = 6,
  bg = "white"
)
