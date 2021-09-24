# Makefile for \analysis 
# by Hannes Datta
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output

###### BUILD COMMANDS ######

all: main marketshare

pre: $(TEMP_DIR)/preclean.txt
main: $(OUTPUT_DIR)/elasticities_main.txt $(TEMP_DIR)/audit_main.txt
marketshare: $(OUTPUT_DIR)/elasticities_marketshare.txt $(OUTPUT_DIR)/audit_marketshare.html

###### Build parts ######

## DATA CLEANING

### Pre-clean the data
$(TEMP_DIR)/preclean.txt: preclean.R ../../derived/output/datasets.txt 
	R --vanilla --args "" < "preclean.R"

## MODEL ESTIMATION

### Main models
$(OUTPUT_DIR)/results_main.txt: analysis_main.R $(TEMP_DIR)/preclean.txt proc_analysis_main.R
	R --vanilla --args "" < "analysis_main.R"

$(OUTPUT_DIR)/elasticities_main.txt: extract_elast_main.R $(OUTPUT_DIR)/results_main.txt
	R --vanilla --args "" < "extract_elast_main.R"

$(TEMP_DIR)/audit_main.txt: $(OUTPUT_DIR)/elasticities_main.txt audit_main_run.R audit_main.Rmd
	R --vanilla --args "" < "audit_main_run.R"

### Market-share attraction model for SBBE computation

$(OUTPUT_DIR)/results_marketshare.RData: analysis_marketshare.R $(TEMP_DIR)/preclean.txt proc_analysis_marketshare.R
	R --vanilla --args "" < "analysis_marketshare.R"

$(OUTPUT_DIR)/elasticities_marketshare.txt: extract_elast_marketshare.R $(OUTPUT_DIR)/results_marketshare.RData sbbe.R
	R --vanilla --args "" < "extract_elast_marketshare.R"

$(OUTPUT_DIR)/audit_marketshare.html: $(OUTPUT_DIR)/results_marketshare.RData audit_marketshare.Rmd
	R -e "rmarkdown::render('audit_marketshare.Rmd', output_file = '../output/audit_marketshare.html')"
