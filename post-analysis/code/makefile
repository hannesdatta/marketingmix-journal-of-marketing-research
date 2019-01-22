# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

all: $(TEMP_DIR)/hdi.csv $(TEMP_DIR)/gdp.csv $(TEMP_DIR)/gci.csv $(TEMP_DIR)/gini.csv $(OUTPUT_DIR)/pca.txt

###### Build parts ######

# Prepare HDI data
$(TEMP_DIR)/hdi.csv: hdi.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "hdi.R"

# Prepare GDP data
$(TEMP_DIR)/gdp.csv: gdp.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "gdp.R"

# Prepare GCI (Global Competitiveness Index data)
$(TEMP_DIR)/gci.csv $(TEMP_DIR)/gci_pca.csv: gci.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "gci.R"

# Prepare Gini coefficients
$(TEMP_DIR)/gini.csv: gini.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "gini.R"

# Conduct PCA
$(OUTPUT_DIR)/pca.txt: pca.R $(TEMP_DIR)/gci_pca.csv
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "pca.R"
