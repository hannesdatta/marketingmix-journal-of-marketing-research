# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######
all: elast sim markdown
elast: $(OUTPUT_DIR)/elasticities.txt 
pre: $(TEMP_DIR)/preclean.csv
results: $(OUTPUT_DIR)/results.RData
sim: $(OUTPUT_DIR)/simulation.RData $(OUTPUT_DIR)/simulation.txt
markdown: $(OUTPUT_DIR)/model_results.html

###### Build parts ######

# Pre-clean the data
$(TEMP_DIR)/preclean.txt: preclean.R ../../derived/output/datasets.txt
	R --vanilla --args "" < "preclean.R"

# Run the model
$(OUTPUT_DIR)/results.RData: analysis.R $(TEMP_DIR)/preclean.txt preclean.R proc_analysis.R
	R --vanilla --args "" < "analysis.R"

# Run the simulation
$(OUTPUT_DIR)/simulation.RData: simulation.R $(OUTPUT_DIR)/results.RData proc_simulate.R
	R --vanilla --args "" < "simulation.R"

# Produce overall statistics on estimated models (e.g., elasticities, carry-over parameters, sign. of copula terms)
$(OUTPUT_DIR)/elasticities.txt: extract_elast.R $(OUTPUT_DIR)/results.RData sbbe.R
	R --vanilla --args "" < "extract_elast.R"

# Augment main results with simulations
$(OUTPUT_DIR)/simulation.txt: extract_sim_elast.R $(OUTPUT_DIR)/results.RData
	R --vanilla --args "" < "extract_sim_elast.R"

$(OUTPUT_DIR)/model_results.html: $(OUTPUT_DIR)/results.RData model_results.Rmd
	R -e "rmarkdown::render('model_results.Rmd', output_file = '../output/model_results.html')"
