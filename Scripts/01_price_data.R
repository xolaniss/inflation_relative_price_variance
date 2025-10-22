# Description
# Price data - Xolani Sibande October 2025
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

# Import and cleaning -------------------------------------------------------------
sheets_names <- excel_sheets(here("Data", "Price_dispersion data.xlsx"))
data  <- 
  read_excel(here("Data", "Price_dispersion data.xlsx"), sheet = sheets_names[3], skip = 0)
weights_tbl <- price_data_tbl |> 
  slice((1:2))
price_data_tbl <- 
  data |> 
  slice(-(1:2)) |> 
  janitor::clean_names() |> 
  mutate(month = as.Date(month, format = "%Y-%m-%d")) |> 
  rename(Date = month) |> 
  dplyr::select(-starts_with("x"))
price_data_names <- read_excel(here("Data", "Price_dispersion data.xlsx"), sheet = sheets_names[8], skip = 0) # will implement this at reporting
 

# EDA ---------------------------------------------------------------
price_data_tbl |> glimpse() 
price_data_tbl |> skimr::skim()

# Graphing ---------------------------------------------------------------
price_data_long_tbl <- 
  price_data_tbl |> 
  pivot_longer(-Date, names_to = "Series", values_to = "Value")

price_data_gg <- 
  price_data_long_tbl |>
  filter(!Series %in% c("rvp", "headline_inflation")) |> 
  fx_plot(variables_color = 45)

rvp_headline_inflation_gg <- 
  price_data_long_tbl |>
  filter(Series %in% c("rvp", "headline_inflation")) |> 
  fx_plot(variables_color = 2)

# Export ---------------------------------------------------------------
artifacts_price_data <- list (
  data = list(
    price_data_tbl = price_data_tbl,
    weights = weights,
    price_data_names = price_data_names
  ),
  plots = list(
    price_data_gg = price_data_gg,
    rvp_headline_inflation_gg = rvp_headline_inflation_gg
  )
)

write_rds(artifacts_price_data, file = here("Outputs", "artifacts_price_data.rds"))


