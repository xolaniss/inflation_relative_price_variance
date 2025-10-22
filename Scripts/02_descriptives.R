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
    "Observations" = n()
  ) |> 
  ungroup()

# Export ---------------------------------------------------------------
artifacts_descriptives <- list (
  desc_stats_tbl = desc_stats_tbl
)

write_rds(artifacts_descriptives, file = here("Outputs", "artifacts_descriptives.rds"))


