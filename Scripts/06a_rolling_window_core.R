# Description
# rolling window regression - Xolani Sibande October 2025
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
roll_modelling <- 
  function(data, window, dep_var){
    roll_spec <-   slidify(.f =  ~lm(..1 ~ ..2 + ..3), 
                           .period = window,
                           .align = "right",
                           .unlist = FALSE,
                           .partial = TRUE
    )
    
    data |> 
      mutate(roll = roll_spec({{dep_var}}, core_inflation, rpd_lag1)) |> 
      mutate(tidy = map(roll, broom::tidy)) |> 
      unnest(cols = tidy) |> 
      dplyr::select(Date, term:estimate, statistic, p.value) |> 
      drop_na() |> 
      pivot_wider(names_from = term, values_from = c(estimate, statistic, p.value)) |> 
      dplyr::select(Date, estimate_..2, statistic_..2) |>
      pivot_longer(-Date, names_to = "Series", values_to = "Value") |>
      mutate(Series = dplyr::recode(
        Series,
        "estimate_..2" = "Estimate",
        "statistic_..2" = "t-statistic"))
  }

# Import -------------------------------------------------------------
rpd_data_tbl <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1)


# Rolling window ---------------------------------------------------------------
rpd_data_modelling_tbl <- 
  rpd_data_tbl |> 
  mutate(
  rpd_lag1 = lag(rpd, 1),
  core_inflation = core_inflation) |> 
  dplyr::select(-headline_inflation) |> 
  drop_na() 

## 2-year window --------------- 
roll_rpd_models_2_year_tbl <- 
  rpd_data_modelling_tbl |> 
  roll_modelling(dep_var = rpd, window = 24) |> 
  filter(Date >= ymd("2011-05-31")) 

roll_rpd_2_year_gg <-
  fx_plot(roll_rpd_models_2_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
  labs(subtitle = "2-year window")

## 5_year window ---------------
roll_rpd_models_5_year_tbl <- 
  rpd_data_modelling_tbl |> 
  roll_modelling(dep_var = rpd, window = 60) |> 
  filter(Date >= ymd("2014-05-31"))

roll_rpd_5_year_gg <- fx_plot(roll_rpd_models_5_year_tbl, variables_color = 2) +
  facet_wrap (. ~ Series, scale = "free", labeller = label_parsed) +
  labs(subtitle = "5-year window") 

## Combined plots ----------------
rpd_combined_core_gg <- 
  roll_rpd_2_year_gg /
  roll_rpd_5_year_gg 

# Export ---------------------------------------------------------------
artifacts_roll_core <- list (
      rpd_combined_gg = rpd_combined_core_gg
  )

write_rds(artifacts_roll_core, file = here("Outputs", "artifacts_roll_core.rds"))


