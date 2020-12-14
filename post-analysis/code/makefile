# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

app: app/app_workspace.RData

lala: app
all: $(OUTPUT_DIR)/final_analysis.Rmd

###### Build parts ######

app/app_workspace.RData: ../../derived/output/covariates.txt app_prep.R $(OUTPUT_DIR)/panel_covariates.txt ../externals/preclean_main.csv proc_auxilary.R proc_rename.R
	R --vanilla < app_prep.R
	
$(OUTPUT_DIR)/panel_covariates.txt: panel_covariates.R ../externals/preclean_main.csv
	R --vanilla < panel_covariates.R


#$(OUTPUT_DIR)/final_analysis.Rmd: final_analysis.Rmd proc_auxilary.R proc_rename.R preclean.R pre
#	"$(RBIN64)Rscript.exe" -e "rmarkdown::render('final_analysis.Rmd', output_file = '../output/final_analysis.html')"
