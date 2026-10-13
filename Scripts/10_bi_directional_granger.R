# Description
# DCC MGARCH attemp

# Preliminaries -----------------------------------------------------------
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
source(here("Functions", "hong.R"))
source(here("Functions", "hong_plot.R"))

# Import -------------------------------------------------------------
rpd_data_tbl <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1) |> 
  janitor::clean_names() |> 
  dplyr::select(date, headline_inflation, rpd) |> 
  mutate(date = as.Date(date))

uinf_data_tbl <- read_excel(here("Data", "unexpected_inflation_data.xlsx")) |> 
  rename(Date = month) |>
  janitor::clean_names() |> 
  dplyr::select(date, hcpi_u, rpd) |> 
  drop_na()


# hong test -----------------------------------------------------------------
dcc_expected_gg <- 
  hong_test(
  data = rpd_data_tbl,
  M = 3,
  var_one_col_selected = "rpd",
  var_two_col_selected = "headline_inflation"
) |> 
  hong_plot(
    line_color = "#81a9ad" ,
    y_var = Hb,
    z_var = phb
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(
    text = element_text(size = 8),
    strip.background = element_rect(colour = "white", fill = "white"),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 8),
    plot.tag = element_text(size = 8),
    axis.line = element_line(color = "black", linewidth = 0.2),
    strip.text = element_blank()
  )

dcc_unexpected_gg <- 
  hong_test(
  data = uinf_data_tbl,
  M = 3,
  var_one_col_selected = "rpd",
  var_two_col_selected = "hcpi_u"
) |> 
  hong_plot(
    line_color = "#81a9ad",
    y_var = Hb,
    z_var = phb
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(
    text = element_text(size = 8),
    strip.background = element_rect(colour = "white", fill = "white"),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 8),
    plot.tag = element_text(size = 8),
    axis.line = element_line(color = "black", linewidth = 0.2),
    strip.text = element_blank()
  )


# Export ---------------------------------------------------------------
artifacts_dcc <- list (
  dcc_expected_gg = dcc_expected_gg,
  dcc_unexpected_gg = dcc_unexpected_gg

)

write_rds(artifacts_dcc, file = here("Outputs", "artifacts_dcc.rds"))


