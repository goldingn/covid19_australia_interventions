# Run the full Reff pipeline

source("R/lib.R")

source("R/functions.R")

#deal with empty ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
# Section A) Independent of NNDSS and survey data update:

# 1. Ingest mobility data, fit mobility models and output figures
# (<state>_datastream_model_fit.png x8 and multistate_model_fit.png), and trend
# estimates (google_change_trends.RDS) [~60s]

source("R/mobility_change.R")
# -- These figs into dropbox/to Freya
# -- outputs/mobility_dates.csv into dropbox/to Freya

# 2. Vaccination effect
# [~ 3 min]
source("R/vacccination_with_waning.R")
# -- Figs into dropbox / to Freya
# -- vaccination_effect_timeseries_<date>.csv to dropbox and mediaflux
# -- effective_dose_data_<date>.csv to mediaflux

# Section B) Dependent on NNDSS data update:
source("R/check_linelist.R")
#  produces notification date by state plots for the most recent 28 days
# optionally, produce NINDSS only watermelon plot

# 3. Run surveillance effect models and output figures (surveillance_effect.png
# and notification_delays.png) and model objects (delay_from_onset_cdfs.RDS)
# [~60s]

#overall linelist read line
linelist <- load_linelist(use_vic = FALSE)#skip Vic when using commonwealth data

# remove dubious SA onset dates
linelist$date_onset[(linelist$state == "SA" & linelist$date_onset >= as_date("2022-02-27"))] <- NA


source("R/rolling_delays.R")
#  -- figs to dropbox / to Freya

# 4. TTIQ and Isolation effect
#source("R/isolation_effect.R")
# -- figs and ttiq_effect.csv to dropbox / to Freya

# 5. Sync NNDSS data and write out case data (local_cases.csv) for Monash (Rob Hyndman/Mitch)
# and U of Melbourne (Rob Moss/Ruarai). RNG seed needs to match that in R_effective.R for imputation to
# be consistent. This is also done in the final script, but I send it to them
# whilst waiting [~60s]
set.seed(2020-04-29)
sync_nndss() # this line probably not necessary for HPC / widows, but shoudn't break aything
data <- reff_model_data(linelist_raw = linelist)
data$dates$linelist  # check it synced properly
# -- is this date the correct linelist date?
write_local_cases(data)
# -- put in dropbox and notify Mitch/Rob J/Ruarai

# Check no entries classed as "ERROR" (i.e. conflicting PLACE_OF_ACQUISITION and CV_SOURCE_INFECTION)
# if OK will only list "imported" and "local"
linelist %>%
  pull(import_status) %>%
  table

# write linelist format for UoAdelaide (Tobin/Josh) if necessary
# usually unnecessary unless edits to raw linelist
write_linelist(linelist = linelist)

# Section C) Dependent on survey data update (the numbered file must be manually
# copied to data/survey_raw):

# 6. parse all contact rate data, automatically remove duplicates, and visually
# check for duplicate ages [~60s]
parse_all_doh_surveys() %>%
  filter(wave > (max(wave) - 4)) %>%
  plot_age_duplication()

ggsave(
  "outputs/figures/age_deduplication_check.png",
  width = 9,
  height = 10
)
# will light up cells with any overly large counts. If this happens check raw data

# 7. Output microdistancing survey data for U of Adelaide (Tobin/Josh, previously Dennis)
# (data/microdistancing/Barometer wave <wave> compliance.csv and
# data/contacts/barometer/contact numbers wave <wave>.csv), run microdistancing
# model, and output figure (microdistancing_effect.png) and trend estimates
# (microdistancing_trends.RDS) [~2 min]
source("R/microdistancing_change.R")
# -- email data/microdistancing/Barometer wave <wave> compliance.csv and
# data/contacts/barometer/contact numbers wave <wave>.csv to Tobin/Dylan/Josh
# -- figure to dropbox / Freya

# 8. Run macrodistancing model and output figure ((microdistancing_effect.png) and
# trend estimates (macrodistancing_trends.RDS). Can be run concurrently with
# microdistancing model (i.e. on a separate process/machine) [~1-2h]
source("R/macrodistancing_change.R")
# -- figure to dropbox / Freya

# Section C) Dependent on NNDSS data update and outputs of all previous scripts:

# 9. Run R effective model and output figures (R_eff_12_local.png,
# R_eff_1_import.png, R_eff_1_local.png, R_eff_1_local_macro.png,
# R_eff_1_local_micro.png, R_eff_1_local_surv.png, R_eff_2_local.png in both
# outputs/figures/ and outputs/projection/figures/) and trajectory draws for Rob
# Moss ( r_eff_12_local_samples_soft_clamped_50.csv, r_eff_1_local_samples.csv,
# r_eff_12_local_samples.csv, r_eff_12_local_samples_soft_clamped_95.csv in both
# outputs/ and outputs/projection/) [~1-2h]
source("R/R_effective.R")
## - figures, samples as above, plus wt, alpha and delta, and
# outputs/output_dates.csv to dropbox/Freya

# hooray - you're done!
