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
rpd_stationarity_tbl <- 
  rpd_data |> 
  pivot_longer(cols = -Date, names_to = "Measure", values_to = "Value") |>
  group_by(Measure) |>
  summarise(
    adf_test = tseries::adf.test(Value)$p.value,
    pp_test = pp.test(Value)$p.value,
    kp_test = kpss.test(Value)$p.value
  ) |> 
  mutate(
    Stationarity = ifelse(adf_test < 0.1 | pp_test < 0.1 | kp_test < 0.1, "Stationary", "Non-Stationary")
  ) |>  # for the rpds could be caused by structural breaks in the data 
  filter(Measure %in% c("rpd", "rpd_division", "headline_inflation")) |> 
  rename(
    "ADF test (p-value)" = adf_test,
    "PP test (p-value)" = pp_test,
    "KPSS test (p-value)" = kp_test
  )

# rpd_stationarity_diff_tbl <- 
#   rpd_tbl |> 
#   pivot_longer(cols = -Date, names_to = "Measure", values_to = "Value") |>
#   group_by(Measure) |>
#   summarise(
#     adf_test = tseries::adf.test(diff(Value))$p.value,
#     pp_test = pp.test(diff(Value))$p.value
#   ) |> 
#   mutate(
#     Stationarity = ifelse(adf_test < 0.05 & pp_test < 0.05, "Stationary", "Non-Stationary")
#   )
  

# Export ---------------------------------------------------------------
artifacts_stationarity <- list (
  rpd_stationarity_tbl = rpd_stationarity_tbl
  # rpd_stationarity_diff_tbl = rpd_stationarity_diff_tbl
)

write_rds(artifacts_stationarity, file = here("Outputs", "artifacts_stationarity.rds"))


