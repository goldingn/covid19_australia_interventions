# Run the full Reff pipeline

source("R/lib.R")

# Section A) Independent of NNDSS and survey data update:

# 1. Ingest mobility data, fit mobility models and output figures
# (<state>_datastream_model_fit.png x8 and multistate_model_fit.png), and trend
# estimates (google_change_trends.RDS) [~60s]
source("R/mobility_change.R")


# Section B) Dependent on NNDSS data update:

source("R/check_linelist.R")

# 2. Run surveillance effect models and output figures (surveillance_effect.png
# and notification_delays.png) and model objects (delay_from_onset_cdfs.RDS)
# [~60s]
source("R/rolling_delays.R")


# 3. Sync NNDSS data and write out case data (local_cases.csv) for the Robs
# Hyndman and Moss. RNG seed needs to match that in R_effective.R for imputation to
# be consistent. This is also done in the final script, but I send it to them
# whilst waiting [~60s]
set.seed(2020-04-29)
sync_nndss()
data <- reff_model_data()
data$dates$linelist  # check it synced properly
write_local_cases(data)


# Section C) Dependent on survey data update (the numbered file must be manually
# copied to data/survey_raw):

# 4. parse all contact rate data, automatically remove duplicates, and visually
# check for duplicate ages [~60s]
parse_all_doh_surveys() %>%
  filter(wave > (max(wave) - 4)) %>%
  plot_age_duplication()

ggsave(
  "outputs/figures/age_deduplication_check.png",
  width = 9,
  height = 10
)

# 5. Output microdistancing survey data for Dennis
# (data/microdistancing/Barometer wave <wave> compliance.csv and
# data/contacts/barometer/contact numbers wave <wave>.csv), run microdistancing
# model, and output figure (microdistancing_effect.png) and trend estimates
# (microdistancing_trends.RDS) [~0.5-1h]
source("R/microdistancing_change.R")

# 6. Run macrodistancing model and output figure ((microdistancing_effect.png) and
# trend estimates (macrodistancing_trends.RDS). Can be run concurrently with
# microdistancing model (i.e. on a separate process/machine) [~1-2h]
source("R/macrodistancing_change.R")


# Section C) Dependent on NNDSS data update and outputs of all previous scripts:

# 7. Run R effective model and output figures (R_eff_12_local.png,
# R_eff_1_import.png, R_eff_1_local.png, R_eff_1_local_macro.png,
# R_eff_1_local_micro.png, R_eff_1_local_surv.png, R_eff_2_local.png in both
# outputs/figures/ and outputs/projection/figures/) and trajectory draws for Rob
# Moss ( r_eff_12_local_samples_soft_clamped_50.csv, r_eff_1_local_samples.csv,
# r_eff_12_local_samples.csv, r_eff_12_local_samples_soft_clamped_95.csv in both
# outputs/ and outputs/projection/) [~1-2h]
source("R/R_effective.R")

