# build an LGA-level gravity model based on FB data

source("R/functions.R")
library(sf)

# load LGAs
vic_lga <- readRDS("data/spatial/vic_lga.RDS")

# get distance matrix
lga_distance <- vic_lga %>%
  st_geometry() %>%
  st_centroid() %>%
  st_distance() %>%
  units::drop_units()

base <- vic_lga %>%
  st_drop_geometry() %>%
  select(lga, lga_code, pop)

# get tibble for pairs of places
pairs <- expand_grid(
  lga_from = vic_lga$lga,
  lga_to = vic_lga$lga
) %>%
  right_join(
    rename_all(base, paste0, "_from")
  ) %>%
  right_join(
    rename_all(base, paste0, "_to")
  ) %>%
  mutate(
    distance = as.vector(lga_distance),
    distance = distance / 1e3
  )

# load Cam's LGA ordering
file <- "data/facebook/VIC_LGA18_OD_matrices_for_Nick_Golding_07072020/LGA_CODE18_sorted.csv"
lga_codes <- read_csv(file, col_names = FALSE)[[1]]

idx <- match(vic_lga$lga_code, lga_codes)

# load aggregated OD matrices
OD_files <- "data/facebook/VIC_LGA18_OD_matrices_for_Nick_Golding_07072020" %>%
  list.files(
    pattern = "^VIC_LGA2018_OD_mat",
    full.names = TRUE
  ) %>%
  tibble(
    filename = .
  ) %>%
  mutate(
    details = basename(filename),
    details = str_remove(details, "^VIC_LGA2018_OD_mat_"),
    details = str_remove(details, ".csv$")
  ) %>%
  mutate(
    timestamp = str_sub(details, -4),
    start_date = str_sub(details, end = 10),
    end_date = str_sub(details, start = 15, end = 24)
  ) %>%
  select(
    -details
    ) %>%
  mutate_at(
    vars(ends_with("_date")),
    as.Date
  ) %>%
  filter(
    start_date != as.Date("2020-06-16")
  )

# load these in, subset to VIC, and turn into a three column matrix
ODs <- lapply(OD_files$filename,
              read_csv,
              col_names = FALSE) %>%
  lapply(as.matrix) %>%
  lapply(`[`, idx, idx) %>%
  lapply(as.vector) %>%
  lapply(
    cbind,
    expand_grid(
      lga_code_from = vic_lga$lga_code,
      lga_code_to = vic_lga$lga_code
    )
  ) %>%
  lapply(
    rename,
    flow = "X[[i]]"
  )

add_meta <- function(idx, OD_list, meta) {
  meta <- meta[idx, ]
  mutate(OD_list[[idx]],
         start_date = meta$start_date,
         end_date = meta$end_date,
         timestamp = meta$timestamp)
}

# get all combinations, compute (censored) numbers of movements, and restrict to
# pre-lockdown and inter-lga movements
OD_data <- lapply(seq_along(ODs),
                  add_meta,
                  ODs,
                  OD_files) %>%
  bind_rows() %>%
  right_join(pairs) %>%
  tibble() %>%
  mutate(
    days = as.numeric(end_date - start_date) + 1,
    count = round(flow * days)
  ) %>%
  filter(
    end_date < as.Date("2020-07-02"),
    lga_from != lga_to
  )

# fit gravity model
# flow ~ days * N1*N2/d
# log(flow) ~ log(N1) + log(N2) - log(d) + log(days)
m <- glm(count ~
           log(pop_from) +
           log(pop_to) +
           log(distance) +
           timestamp,
         offset = log(days),
         data = OD_data,
         family = stats::poisson)

pred <- pairs %>%
  mutate(
    timestamp = "0000",
    days = 1
  ) %>%
  mutate(
    expected_flow = predict(m, newdata = ., type = "response")
  ) %>%
  mutate(
    expected_flow = ifelse(
      is.finite(expected_flow),
      expected_flow,
      NA
    )
  ) %>%
  select(lga_from,
         lga_to,
         expected_flow) %>%
  pivot_wider(
    names_from = lga_to,
    values_from = expected_flow
  ) %>%
  select(-lga_from) %>%
  as.matrix() %>%
  `rownames<-`(colnames(.))

saveRDS(pred, "data/facebook/baseline_gravity_movement.RDS")


