# summarise postcode level vaccination effect in LGAs of concern
source("R/functions.R")

# load ABS postcode to LGA concordance
postcode_lga_lookup <- read_xlsx(
  "data/spatial/abs/CA_POSTCODE_2018_LGA_2018.xlsx",
  sheet = 4,
  skip = 5
) %>%
  filter(
    row_number() > 1,
    !is.na(RATIO)
  ) %>%
  select(
    postcode = POSTCODE_2018...1,
    lga = LGA_NAME_2018,
    weight = RATIO
  ) %>%
  # normalise weights
  group_by(postcode) %>%
  mutate(
    weight = weight / sum(weight)
  )

# read in postcode-lga lookup and weights
weights_tbl <-  %>%
  # subset to observed postcodes
  old_right_join(
    tibble(
      postcode = postcodes,
    )
  ) %>%
  # assign unrecognised/unknown/overseas postcodes to a separate class
  mutate(
    lga = replace_na(lga, "other"),
    weight = replace_na(weight, 1)
  )


# get postcode total- and age-specific-populations:

# load in meshblock populations
mb_pops <- read_csv(
  "data/spatial/abs/2016 census mesh block counts.csv",
  col_types = cols(
    MB_CODE_2016 = col_double(),
    MB_CATEGORY_NAME_2016 = col_character(),
    AREA_ALBERS_SQKM = col_double(),
    Dwelling = col_double(),
    Person = col_double(),
    State = col_double()
  )
)

# load in meshblock to postcode concordance
poa_mb <- read_csv(
  "data/spatial/abs/POA_2016_AUST.csv",
  col_types = cols(
    MB_CODE_2016 = col_double(),
    POA_CODE_2016 = col_character(),
    POA_NAME_2016 = col_character(),
    AREA_ALBERS_SQKM = col_double()
  )
)

# sum populations by postcode
postcode_pop <- poa_mb %>%
  select(
    -AREA_ALBERS_SQKM
  ) %>%
  left_join(
    mb_pops,
    by = "MB_CODE_2016"
  ) %>%
  select(
    postcode = POA_NAME_2016,
    population = Person
  ) %>%
  group_by(
    postcode
  ) %>%
  summarise(
    across(
      population,
      sum
    )
  )

# join LGA age-populations to LGAs
# normalise LGA age-populations to get approximate age distributions
postcode_age_distribution <- postcode_lga_lookup %>%
  full_join(
    lga_age_population(),
    by = c(lga = "LGA_NAME19")
  ) %>%
  filter(
    !is.na(weight),
    !is.na(population)
  ) %>%
  group_by(
    postcode, age
  ) %>%
  # get LGA-level weighted age population by postcode
  summarise(
    population = sum(population * weight),
    .groups = "drop"
  ) %>%
  # convert to populations to fractions
  group_by(
    postcode
  ) %>%
  mutate(
    fraction = population / sum(population)
  ) %>%
  select(
    -population
  )
  
# check fractions sum to 1
postcode_age_distribution %>%
  group_by(postcode) %>%
  summarise(
    total = sum(fraction)
  ) %>%
  summarise(max(abs(total - 1)))
  
# join age distributions to populations and multiply to get approximate postcode age populations 
postcode_age_pop <- postcode_pop %>%
  full_join(
    postcode_age_distribution,
    by = "postcode"
  ) %>%
  mutate(
    population = population * fraction
  ) %>%
  select(
    -fraction
  ) %>%
  left_join(
    age_lookup,
    by = c("age" = "age_abs"),
  )






# then:
# join on postcode vaccination data

# reset previous vaccination data script (fixing repeat age group issue?)



