# Description
# Descriptives table - Xolani Sibande October 2025
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
price_data_tbl <- read_rds(here("Outputs", "artifacts_price_data.rds")) |> pluck(1,1)

# Descriptive statistics -----------------------------------------------------------------
# Skewness and (excess) kurtosis follow psych::skew()/kurtosi() (type = 2,
# the standard moment-based definition); the Jarque-Bera test jointly
# tests the null of normality (skewness = 0, excess kurtosis = 0).
desc_stats_tbl <- price_data_tbl |> 
  pivot_longer(cols = -Date, names_to = "Goods category", values_to = "inflation_rate") |> 
  filter(!`Goods category` %in% c("rvp", "headline_inflation")) |> 
  group_by(`Goods category`) |> 
  summarise(
    "Mean" = mean(inflation_rate, na.rm = TRUE),
    "Median" = median(inflation_rate, na.rm = TRUE),
    "SD" = sd(inflation_rate, na.rm = TRUE),
    "Min" = min(inflation_rate, na.rm = TRUE),
    "Max" = max(inflation_rate, na.rm = TRUE),
    "Skewness" = psych::skew(inflation_rate, na.rm = TRUE, type = 2),
    "Kurtosis" = psych::kurtosi(inflation_rate, na.rm = TRUE, type = 2),
    "Jarque-Bera" = tseries::jarque.bera.test(na.omit(inflation_rate))$statistic,
    "Jarque-Bera (p-value)" = tseries::jarque.bera.test(na.omit(inflation_rate))$p.value,
    "Observations" = n()
  ) |> 
  mutate(
    stars = if_else(`Jarque-Bera (p-value)` < 0.1, "*", 
                            if_else(`Jarque-Bera (p-value)` < 0.05, "**",
                            if_else(`Jarque-Bera (p-value)` < 0.01, "*", " ")))
  ) |> 
  mutate(`Jarque-Bera` = round(`Jarque-Bera`, 0)) |> 
  mutate(`Jarque-Bera` = paste0(`Jarque-Bera`, stars)) |> 
  dplyr::select(-stars, - `Jarque-Bera (p-value)`)

# Export ---------------------------------------------------------------
artifacts_descriptives <- list (
  desc_stats_tbl = desc_stats_tbl
)

write_rds(artifacts_descriptives, file = here("Outputs", "artifacts_descriptives.rds"))


