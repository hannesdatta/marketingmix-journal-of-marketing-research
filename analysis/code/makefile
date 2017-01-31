# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

import: $(TEMP_DIR)/preclean.dta

###### DATA ANALYSIS ######

# Perform matching
$(TEMP_DIR)/preclean.dta: import.do $(TEMP_DIR)/preclean.csv
	$(STATABIN) /e do import.do


# analyze data
run : analysis.R proc_analysis.R proc_sur.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"
