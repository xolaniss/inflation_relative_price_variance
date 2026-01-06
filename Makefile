# Makefile to run the analysis for the paper and compile the manuscript

## Recursively look for all files in the current directory and its subdirectories
VPATH = $(shell find . -type d)

## List of inputs
INPUT_TARGETS = artifacts_price_data.rds \
artifacts_descriptives.rds \
artifacts_rvp_measures.rds \
artifacts_stationarity.rds \
artifacts_base_regression.rds \
artifacts_roll.rds \
artifacts_recursive.rds \
artifacts_quant_reg.rds


## Generating the manuscript 
mortgage_paper: inflation_relative_price_variance.qmd $(INPUT_TARGETS)
	quarto render $<
	
## Generating rds inputs to manuscript
artifacts_price_data.rds: Scripts/01_price_data.R \
Data/Price_dispersion_data.xlsx
	Rscript $<

artifacts_descriptives.rds: Scripts/02_descriptives.R \
artifacts_price_data.rds
	Rscript $<
	
artifacts_rvp_measures.rds: Scripts/03_rvp_measures.R \
artifacts_price_data.rds
	Rscript $<

artifacts_stationarity.rds: Scripts/04_stationarity_tests.R \
artifacts_rvp_measures.rds
	Rscript $<

artifacts_base_regression.rds: Scripts/05_base_regression.R \
artifacts_rvp_measures.rds
	Rscript $<

artifacts_roll.rds: Scripts/06_rolling_window.R \
artifacts_rvp_measures.rds
	Rscript $<

artifacts_recursive.rds: Scripts/07_recursive_regressions.R \
artifacts_rvp_measures.rds
	Rscript $<

artifacts_quant_reg.rds: Scripts/08_quant_reg.R \
artifacts_rvp_measures.rds
	Rscript $<