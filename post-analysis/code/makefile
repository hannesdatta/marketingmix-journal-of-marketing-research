# Makefile for the \post-analysis module
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output
EXT_DIR = ../externals

###### BUILD COMMANDS ######
all: $(OUTPUT_DIR)/tables_and_figures_main.html \
     $(OUTPUT_DIR)/tables_and_figures_appendix.html \
     $(OUTPUT_DIR)/tables_and_figures_other.log \
	 $(OUTPUT_DIR)/tables_and_figures_boxplot.txt \
	 $(OUTPUT_DIR)/tables_and_figures_holdout.txt

build: $(OUTPUT_DIR)/workspace.RData


###### Build workspace w/ all results ######

$(OUTPUT_DIR)/panel_covariates.txt: panel_covariates.R ../externals/preclean_main.csv
	R --vanilla < panel_covariates.R

$(OUTPUT_DIR)/workspace.RData $(OUTPUT_DIR)/predictions_kfold.csv $(OUTPUT_DIR)/predictions_within.csv: ../../derived/output/covariates.txt build_workspace.R $(OUTPUT_DIR)/panel_covariates.txt $(EXT_DIR)/preclean_main.csv proc_auxilary.R proc_rename.R
	R --vanilla < build_workspace.R
	
###### Generate tables and figures ######

$(OUTPUT_DIR)/tables_and_figures_main.html: tables_and_figures_main.Rmd $(OUTPUT_DIR)/workspace.RData
	R -e "rmarkdown::render('tables_and_figures_main.Rmd', output_file = '../output/tables_and_figures_main.html')"

$(OUTPUT_DIR)/tables_and_figures_appendix.html: tables_and_figures_appendix.Rmd $(OUTPUT_DIR)/workspace.RData ../output/predictions_kfold.csv ../output/predictions_within.csv proc_auxilary.R proc_rename.R
	R -e "rmarkdown::render('tables_and_figures_appendix.Rmd', output_file = '../output/tables_and_figures_appendix.html')"

$(OUTPUT_DIR)/tables_and_figures_other.log $(OUTPUT_DIR)/tables_and_figures_other.txt: tables_and_figures_other.R $(OUTPUT_DIR)/workspace.RData ../output/predictions_kfold.csv ../output/predictions_within.csv proc_auxilary.R
	R --vanilla < tables_and_figures_other.R > $(OUTPUT_DIR)/tables_and_figures_other.txt

$(OUTPUT_DIR)/tables_and_figures_boxplot.txt: tables_and_figures_boxplot.R ../externals/preclean_main.csv proc_auxilary.R
	R --vanilla < tables_and_figures_boxplot.R

$(OUTPUT_DIR)/tables_and_figures_holdout.txt: tables_and_figures_holdout.R ../output/predictions_within.csv proc_auxilary.R
	R --vanilla < tables_and_figures_holdout.R