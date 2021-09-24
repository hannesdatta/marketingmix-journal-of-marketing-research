# Makefile for the \post-analysis module
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output
EXT_DIR = ../externals

###### BUILD COMMANDS ######

build: $(OUTPUT_DIR)/workspace.RData

all: $(OUTPUT_DIR)/final_analysis.Rmd

###### Build workspace w/ all results ######

$(OUTPUT_DIR)/panel_covariates.txt: panel_covariates.R ../externals/preclean_main.csv
	R --vanilla < panel_covariates.R

$(OUTPUT_DIR)/workspace.RData $(OUTPUT_DIR)/predictions_kfold.csv $(OUTPUT_DIR)/predictions_within.csv: ../../derived/output/covariates.txt build_workspace.R $(OUTPUT_DIR)/panel_covariates.txt $(EXT_DIR)/preclean_main.csv proc_auxilary.R proc_rename.R
	R --vanilla < build_workspace.R
	
###### Generate tables and figures ######


app/tables.html: new_results.Rmd app/app_workspace.RData ../output/predictions_kfold.csv ../output/predictions_within.csv proc_auxilary.R proc_rename.R
	R -e "rmarkdown::render('new_results.Rmd', output_file = 'app/tables.html')"
