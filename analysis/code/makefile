# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######
all: elast markdown
elast: $(OUTPUT_DIR)/elasticities.txt
pre: $(TEMP_DIR)/preclean.csv
results: $(TEMP_DIR)/results.RData
markdown: $(OUTPUT_DIR)/model_results.html

###### Build parts ######

# Pre-clean the data
$(TEMP_DIR)/preclean.csv: preclean.R ../../derived/output/datasets.csv
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "preclean.R"

# Run the model
$(OUTPUT_DIR)/results.RData: analysis.R $(TEMP_DIR)/preclean.csv preclean.R proc_analysis.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"

# Produce overall statistics on estimated models (e.g., elasticities, carry-over parameters, sign. of copula terms)
$(OUTPUT_DIR)/elasticities.txt: extract_elast.R $(OUTPUT_DIR)/results.RData sbbe.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "extract_elast.R"

$(OUTPUT_DIR)/model_results.html: $(OUTPUT_DIR)/results.RData model_results.Rmd
	"$(RBIN64)Rscript.exe" -e "rmarkdown::render('model_results.Rmd', output_file = '../output/model_results.html')"
