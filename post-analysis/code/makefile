# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

app: app/app_workspace.RData app/tables.html

lala: app
all: $(OUTPUT_DIR)/final_analysis.Rmd

###### Build parts ######

app/app_workspace.RData: ../../derived/output/covariates.txt app_prep.R $(OUTPUT_DIR)/panel_covariates.txt ../externals/preclean_main.csv proc_auxilary.R proc_rename.R
	R --vanilla < app_prep.R
	
$(OUTPUT_DIR)/panel_covariates.txt: panel_covariates.R ../externals/preclean_main.csv
	R --vanilla < panel_covariates.R

app/tables.html: new_results.Rmd app/app_workspace.RData proc_auxilary.R proc_rename.R
	R -e "rmarkdown::render('new_results.Rmd', output_file = 'app/tables.html')"
