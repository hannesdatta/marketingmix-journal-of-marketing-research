# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

all: $(TEMP_DIR)/hdi.csv $(TEMP_DIR)/gdp.csv $(TEMP_DIR)/gci.csv

###### Build parts ######

# Prepare HDI data
$(TEMP_DIR)/hdi.csv: hdi.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "hdi.R"

# Prepare GDP data
$(TEMP_DIR)/gdp.csv: gdp.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "gdp.R"

# Prepare GCI (Global Competitiveness Index data)
$(TEMP_DIR)/gci.csv: gci.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "gci.R"
