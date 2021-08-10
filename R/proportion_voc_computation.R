source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R/functions", full.names = TRUE), source)

# this from reff_model_Data()
linelist_raw <- load_linelist(use_vic = FALSE)
linelist_date <- max(linelist_raw$date_linelist)

notification_delay_cdf <- get_notification_delay_cdf(linelist_raw)

linelist <- linelist_raw %>%
  impute_linelist(notification_delay_cdf = notification_delay_cdf)

earliest_date <- min(linelist$date)
latest_date <- max(linelist$date)
states <- sort(unique(linelist$state))
dates <- seq(earliest_date, latest_date, by = 1)

# and those infected in any state, but infectious in this one
local_cases_infectious <- linelist %>%
  infections_by_region(
    region_type = "state",
    case_type = "local"
  )

# make fake strain data, starting with case counts by date of infectiousness, in
# the state where they are infectious
# note the dates are nonsense - I only had NNDSS up to Jan loaded

all <- local_cases_infectious
wt <- all
wt[dates > as.Date("2020-12-01"), ] <- 0
non_wt <- all - wt

# split out alpha
kappa_delta <- non_wt * 0
kappa_delta[dates > as.Date("2020-12-14"), states == "VIC"] <- non_wt[dates > as.Date("2020-12-14"), states == "VIC"]
alpha <- non_wt - kappa_delta

# split apart kappa and delta
delta <- kappa_delta * 0
delta[dates > as.Date("2020-12-27"), ] <- kappa_delta[dates > as.Date("2020-12-27"), ] * 0.5
kappa <- kappa_delta - delta

# check I did this right
identical(all, wt + alpha + delta + kappa)

# image(log1p(wt))
# image(log1p(alpha))
# image(log1p(kappa))
# image(log1p(delta))

# compute porportions by case - inputs are the equivalents of local_infectious in reff_model_data

props <- voc_proportions(
  wt_prop = wt,
  alpha_prop = alpha,
  delta_prop = delta,
  kappa_prop = kappa
)

image(props$wt_prop)
image(props$alpha_prop)
image(props$delta_prop)
image(props$kappa_prop)

# mmanually set default proportions for when there are no infectious cases

# need to do this after computing proportions (not on cases, since GI
# convolution would make it bleed into later dates)

# get masks to set proportions where needed
need_default <- Reduce("+", props) == 0
use_wt_default <- use_alpha_default <- need_default
use_wt_default[dates > as.Date("2020-12-14"), ] <- FALSE
use_alpha_default[dates <= as.Date("2020-12-14"), ] <- FALSE

# assign them to relevant variants
props$wt_prop[use_wt_default] <- 1
props$alpha_prop[use_alpha_default] <- 1

# recompute total to check (some small numerical error is to be expected)
total_props <- Reduce("+", props)
max(abs(total_props - 1))

