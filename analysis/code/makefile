# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

import: $(TEMP_DIR)/preclean.csv

###### DATA ANALYSIS ######

# Compute instruments
$(TEMP_DIR)/preclean.csv: preclean.R ../../derived/output/datasets.csv
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "preclean.R"

# xxxxx
#$(TEMP_DIR)/preclean.dta: import.do $(TEMP_DIR)/preclean.csv
#	$(STATABIN) /e do import.do
