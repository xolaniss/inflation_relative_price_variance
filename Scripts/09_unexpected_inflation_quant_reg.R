# Description
# Quantile regressions - Xolani Sibande 2025
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
library(quantreg)
library(np)
library(KernSmooth)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "QQR_functions.R"))

# Import -------------------------------------------------------------
uinf_data_tbl <- read_excel(here("Data", "unexpected_inflation_data.xlsx")) |> 
  rename(Date = month) |>
  drop_na()

# QQR ---------------------------------------------------------------------
## Full sample QQR -----------------------------------------------------------
qqr_rpd_full <- QQR(x = uinf_data_tbl $hcpi_u,
                    y = log(uinf_data_tbl $rpd),
                    hm = "CV"
)
qqr_rpd_full_gg <- ggplot.QQR(qqr_rpd_full)

## Pre 2017 QQR -----------------------------------------------------------
rpd_data_pre_tbl <- uinf_data_tbl  |> filter(Date < "2017-01-01")
qqr_rpd_pre_2017 <-QQR(x = rpd_data_pre_tbl$hcpi_u,
                       y = log(rpd_data_pre_tbl$rpd),
                       hm = "CV"
)
qqr_rpd_pre_2017_gg <- ggplot.QQR(qqr_rpd_pre_2017)

## Post 2017 QQR -----------------------------------------------------------
rpd_data_post_tbl <- uinf_data_tbl  |> filter(Date >= "2017-01-01")
qqr_rpd_post_2017 <-QQR(x = rpd_data_post_tbl$hcpi_u,
                        y = log(rpd_data_post_tbl$rpd),
                        hm = "CV"
)
qqr_rpd_post_2017_gg <- ggplot.QQR(qqr_rpd_post_2017)


# Export ---------------------------------------------------------------
artifacts_uinf_quant_reg <- list (
  qqr_rpd_full_gg = qqr_rpd_full_gg,
  qqr_rpd_pre_2017_gg = qqr_rpd_pre_2017_gg,
  qqr_rpd_post_2017_gg = qqr_rpd_post_2017_gg
)

write_rds(artifacts_uinf_quant_reg, file = here("Outputs", "artifacts_uinf_quant_reg.rds"))


