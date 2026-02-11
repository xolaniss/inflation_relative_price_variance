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

library(slider)
library(timetk)


# Import -------------------------------------------------------------
rpd_data_tbl <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1)

# Data ---------------------------------------------------------------
rpd_data_modelling_tbl <- 
  rpd_data_tbl |> 
  mutate(
    rpd_lag1 = lag(rpd, 1),
    rpd_lag2 = lag(rpd, 2),
    rpd_lag3 = lag(rpd, 3)
    ) |> 
  drop_na() 

# Recursive regression function -------------------------

## Recursive regression ------------------------------
results <- tibble()
data <- rpd_data_modelling_tbl
data[40, ]

for(i in 40:nrow(data)) {
  train_data <- data[1:i, ]
  
  model <- lm(log(rpd) ~ headline_inflation + log(rpd_lag1) + log(rpd_lag2) + log(rpd_lag3), data = train_data)
  
  results <- bind_rows(
    results,
    tibble(
      date = data$Date[i],
      headline_inflation = coef(model)["headline_inflation"],
      rpd_lag1 = coef(model)["rpd_lag1"],
      rpd_lag2 = coef(model)["rpd_lag2"],
      rpd_lag3 = coef(model)["rpd_lag3"],
      intercept = coef(model)["(Intercept)"]
    )
  )
}

results_gg <- 
  results |> 
  pivot_longer(-date, names_to = "Series", values_to = "Value") |> 
  rename(Date = date) |> 
  filter(Series == "headline_inflation") |> 
  fx_plot(variables_color = 1) +
  labs(title = "",
       x = " ",
       y = "Estimate"
       ) +
  theme(strip.text = element_blank())

# Export ---------------------------------------------------------------
artifacts_recursive <- list (
  results_gg = results_gg
)

write_rds(artifacts_recursive, file = here("Outputs", "artifacts_recursive.rds"))


