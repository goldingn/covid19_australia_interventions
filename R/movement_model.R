# build an LGA-level gravity model based on FB data
source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)

# load Cam's LGA ordering
file <- "data/facebook/VIC_LGA18_OD_matrices_for_Nick_Golding_07072020/LGA_CODE18_sorted.csv"
lga_codes <- read_csv(file, col_names = FALSE)[[1]]

# run model for VIC
parse_OD_files(
  dir = "data/facebook/VIC_LGA18_OD_matrices_for_Nick_Golding_07072020/",
  starts = "VIC_LGA2018_OD_mat_"
) %>%
  build_gravity_matrix(
    lga = readRDS("data/spatial/vic_lga.RDS"),
    lga_codes = lga_codes,
    end = as.Date("2020-07-02")
  ) %>%
  saveRDS("data/facebook/vic_baseline_gravity_movement.RDS")

# and for NSW & ACT
nsw_lga <- readRDS("data/spatial/nsw_lga.RDS")
act_lga <- readRDS("data/spatial/act_lga.RDS")
nsw_act_lga <- st_union(
  nsw_lga,
  act_lga
)

parse_OD_files(
  dir = "data/facebook/OD_matrices_for_Nick_Golding_2020_07_15/NSW",
  starts = "NSW_LGA2018_OD_mat_from_tiles_"
) %>%
  build_gravity_matrix(
    lga = nsw_act_lga,
    lga_codes = lga_codes,
    end = as.Date("2020-07-02")
  ) %>%
  saveRDS("data/facebook/nsw_act_baseline_gravity_movement.RDS")
