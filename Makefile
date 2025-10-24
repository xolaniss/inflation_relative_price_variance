# Makefile to run the analysis for the paper and compile the manuscript

## Recursively look for all files in the current directory and its subdirectories
VPATH = $(shell find . -type d)

## List of inputs
INPUT_TARGETS = artifacts_price_data.rds \
artifacts_descriptives.rds \
artifacts_rvp_measures.rds

## Generating the manuscript 
mortgage_paper: inflation_relative_price_variance.qmd $(INPUT_TARGETS)
	quarto render $<
	
## Generating rds inputs to manuscript
artifacts_price_data.rds: 01_price_data.R \
Price_dispersion_data.xlsx
	Rscript $<

artifacts_descriptives.rds: 02_descriptives.R \
artifacts_price_data.rds
	Rscript $
	
artifacts_rvp_measures.rds: 03_rvp_measures.R \
artifacts_price_data.rds
	Rscript $<
	