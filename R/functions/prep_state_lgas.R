# prep a spatial layer with Victorian LGAs and their populations
prep_state_lgas <- function(
  state = "Victoria",
  out_dir = "data/spatial"
) {
  
  state_short <- abbreviate_states(state)
  filepath <- file.path(out_dir,
                        paste0(
                          tolower(state_short),
                          "_lga.RDS"
                        ))
  library(sf)
  
  # load populations of all meshblocks
  mesh_pop <- read_csv(
    "data/spatial/abs/2016 census mesh block counts.csv",
    col_types = cols(
      MB_CODE_2016 = col_character(),
      MB_CATEGORY_NAME_2016 = col_character(),
      AREA_ALBERS_SQKM = col_double(),
      Dwelling = col_double(),
      Person = col_double(),
      State = col_double()
    )
  ) %>%
    rename(
      MB_CODE16 = MB_CODE_2016
    )
  
  # add populations onto shapefile
  state_mesh <- paste0(
    "data/spatial/abs/MB_2016_",
    state_short,
    ".shp"
  ) %>%
    st_read(
      stringsAsFactors = FALSE
    ) %>%
    left_join(mesh_pop)
  
  # get LGAs in VIC, join with mesh blocks, and sum populations
  st_read("data/spatial/abs/LGA_2016_AUST.shp",
          stringsAsFactors = FALSE) %>%
    filter(STE_NAME16 == state) %>%
    select(lga_code = LGA_CODE16,
           lga = LGA_NAME16,
           area = AREASQKM16) %>%
    st_join(state_mesh) %>%
    group_by(lga_code, lga, area) %>%
    summarise(pop = sum(Person)) %>%
    filter(area > 0) %>%
    mutate(
      pop_dens = pop / area
    ) %>%
    saveRDS(filepath)
  
}
