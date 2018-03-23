# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

hdidat: $(TEMP_DIR)/hdi.csv
gdpdat: $(TEMP_DIR)/gdp.csv

###### Build parts ######

# Prepare HDI data
$(TEMP_DIR)/hdi.csv: hdi.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "hdi.R"

# Prepare GDP data
$(TEMP_DIR)/gdp.csv: gdp.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "gdp.R"
