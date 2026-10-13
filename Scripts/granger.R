# Description
# granger causality - XS sept 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
rpd_data_tbl <- read_rds(here("Outputs", "artifacts_rpd_measures.rds")) |> 
  pluck(1)


# Data ---------------------------------------------------------------
rpd_data_modelling_tbl <- 
  rpd_data_tbl |> 
  mutate(
    headline_inflation = headline_inflation) |> 
  drop_na() 

lmtest::grangertest(rpd ~ headline_inflation, order = 3,  data = rpd_data_modelling_tbl |>  filter(Date < "2017-06-01"))
lmtest::grangertest(headline_inflation ~ rpd, order = 3, data = rpd_data_modelling_tbl |>  filter(Date < "2017-06-01"))

# Transformations --------------------------------------------------------


# EDA ---------------------------------------------------------------


# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


