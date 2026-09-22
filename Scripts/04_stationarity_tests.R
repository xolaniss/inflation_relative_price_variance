# Description
# Stationarity tests - Xolani Sibande 
# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)
library(qs2)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Parallel processing
library(furrr)
library(parallel)
library(tictoc)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
rpd_data <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1)

#???Is the data seasonaly adjusted?


# Structural breaks -----------------------------------------------------
breakpoints_rpd <- breakpoints(rpd ~ 1, data = rpd_data)
summary(breakpoints_rpd)
plot(breakpoints_rpd)

breakpoints_inflation <- breakpoints(headline_inflation ~ 1, data = rpd_data)
summary(breakpoints_inflation)
plot(breakpoints_inflation)

# Stationarity ------------------------------------------------------------
# Note: ADF/PP test H0: unit root (low p-value => stationary), while KPSS
# tests H0: stationary (low p-value => non-stationary). These rules must
# NOT be combined with a single OR condition on the same p-value threshold.
rpd_stationarity_tbl <- 
  rpd_data |> 
  pivot_longer(cols = -Date, names_to = "Measure", values_to = "Value") |>
  group_by(Measure) |>
  summarise(
    adf_test = tseries::adf.test(Value)$p.value,
    pp_test = pp.test(Value)$p.value,
    kp_test = kpss.test(Value)$p.value
  ) |> 
  # mutate(
  #   Stationarity = ifelse(adf_test < 0.1 & pp_test < 0.1 & kp_test < 0.1, "Stationary", "Non-Stationary")
  # ) |>  # for the rpds could be caused by structural breaks in the data 
  filter(Measure %in% c("rpd", "rpd_division", "headline_inflation")) |> 
  rename(
    "ADF test (p-value)" = adf_test,
    "PP test (p-value)" = pp_test,
    "KPSS test (p-value)" = kp_test
  )

# Zivot-Andrews single-endogenous-break unit root test -------------------
# ADF, PP, and KPSS assume no structural break under the null. Since
# breakpoints() above finds multiple breaks in both series, we re-test
# allowing for one endogenous break in both intercept and trend (Zivot and
# Andrews, 1992), which nests the Perron (1989) fixed-break test but
# estimates the break date from the data rather than assuming it.
za_test <- function(x) {
  fit <- urca::ur.za(x, model = "both", lag = NULL)
  tibble(
    za_statistic = fit@teststat,
    za_break_obs = fit@bpoint,
    za_break_date = rpd_data$Date[fit@bpoint],
    za_cv_1pct = fit@cval[1],
    za_cv_5pct = fit@cval[2],
    za_cv_10pct = fit@cval[3]
  )
}

rpd_za_tbl <-
  rpd_data |>
  # select(headline_inflation, rpd) |>
  map(za_test) |>
  list_rbind(names_to = "Measure") |>
  mutate(
    Stationarity = ifelse(za_statistic < za_cv_10pct, "Stationary", "Non-Stationary")
  )

# Nonlinearity tests (BDS) ------------------------------------------------
# The BDS test (Broock, Scheinkman, Dechert and LeBaron, 1996) tests the
# null of i.i.d. against unspecified (including nonlinear) dependence. It
# is applied to residuals from a linear AR(3) filter -- matching the 3
# lags used in @eq-rpd_reg -- so that any remaining rejection reflects
# nonlinear dependence rather than simple linear autocorrelation.
ar_hcpi <- arima(rpd_data$headline_inflation, order = c(3, 0, 0))
ar_rpd  <- arima(rpd_data$rpd, order = c(3, 0, 0))

# Check that linear structure has been adequately removed before
# interpreting BDS results as evidence of nonlinearity
whiteness_hcpi <- Box.test(residuals(ar_hcpi), lag = 12, type = "Ljung-Box")
whiteness_rpd  <- Box.test(residuals(ar_rpd),  lag = 12, type = "Ljung-Box")

bds_test <- function(resid) {
  fit <- tseries::bds.test(as.numeric(resid))
  as_tibble(fit$p.value, rownames = "embedding_dim") |>
    pivot_longer(-embedding_dim, names_to = "epsilon", values_to = "p_value") |>
    mutate(embedding_dim = parse_number(embedding_dim))
}

rpd_bds_tbl <-
  list(
    headline_inflation = residuals(ar_hcpi),
    rpd = residuals(ar_rpd)
  ) |>
  map(bds_test) |>
  list_rbind(names_to = "Measure")


# Export ---------------------------------------------------------------
artifacts_stationarity <- list (
  rpd_stationarity_tbl = rpd_stationarity_tbl,
  rpd_za_tbl = rpd_za_tbl,
  rpd_bds_tbl = rpd_bds_tbl
  # rpd_stationarity_diff_tbl = rpd_stationarity_diff_tbl
)

write_rds(artifacts_stationarity, file = here("Outputs", "artifacts_stationarity.rds"))


