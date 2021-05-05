# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######
all: main marketshare
#sales
#marketshare  sales

pre: $(TEMP_DIR)/preclean.txt

marketshare: $(OUTPUT_DIR)/elasticities_marketshare.txt $(OUTPUT_DIR)/audit_marketshare.html
sales: $(OUTPUT_DIR)/elasticities_sales.txt $(OUTPUT_DIR)/audit_sales.html
main: $(OUTPUT_DIR)/elasticities_main.txt $(TEMP_DIR)/explore_main.txt

###### Build parts ######

## DATA CLEANING

### Pre-clean the data
$(TEMP_DIR)/preclean.txt: preclean.R ../../derived/output/datasets.txt 
	R --vanilla --args "" < "preclean.R"

## MODEL ESTIMATION

#####################################
### Market-share attraction model ###
#####################################

#### Model estimation
$(OUTPUT_DIR)/results_marketshare.RData: analysis_marketshare.R $(TEMP_DIR)/preclean.txt proc_analysis_marketshare.R
	R --vanilla --args "" < "analysis_marketshare.R"

#### Produce overall statistics on estimated models (e.g., elasticities, carry-over parameters, sign. of copula terms)
$(OUTPUT_DIR)/elasticities_marketshare.txt: extract_elast_marketshare.R $(OUTPUT_DIR)/results_marketshare.RData sbbe.R
	R --vanilla --args "" < "extract_elast_marketshare.R"

#### Model diagnostics
$(OUTPUT_DIR)/audit_marketshare.html: $(OUTPUT_DIR)/results_marketshare.RData audit_marketshare.Rmd
	R -e "rmarkdown::render('audit_marketshare.Rmd', output_file = '../output/audit_marketshare.html')"

###################
### Main models ###
###################

#### Model estimation
$(OUTPUT_DIR)/results_main.txt: analysis_main.R $(TEMP_DIR)/preclean.txt proc_analysis_main.R
	R --vanilla --args "" < "analysis_main.R"

#### 
$(OUTPUT_DIR)/elasticities_main.txt: extract_elast_main.R $(OUTPUT_DIR)/results_main.txt
	R --vanilla --args "" < "extract_elast_main.R"

$(TEMP_DIR)/explore_main.txt: $(OUTPUT_DIR)/elasticities_main.txt run_explore_main.R explore_main.Rmd
	R --vanilla --args "" < "run_explore_main.R"


