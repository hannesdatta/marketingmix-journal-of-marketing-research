# Makefile for \derived of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output
AUDIT_DIR = ../audit

###### BUILD COMMANDS ######

# full build
all: build audit

build: $(OUTPUT_DIR)/datasets.txt

# audit
audit: $(TEMP_DIR)/audit_rawplot.txt $(TEMP_DIR)/audit_markets.txt $(TEMP_DIR)/audit_brands.txt $(TEMP_DIR)/audit_attributes.txt $(TEMP_DIR)/audit_advertising.txt

# building parts of the project
unzip: $(TEMP_DIR)/unzipped.RData
categorize: $(TEMP_DIR)/categorized.RData
gdponly: $(TEMP_DIR)/gdppercap.RData
uniq: $(TEMP_DIR)/uniqueness_and_lagsales.RData
exatt: $(TEMP_DIR)/extract_attributes.RData 
setatt: $(TEMP_DIR)/attributes.RData
metr: $(TEMP_DIR)/brand_metrics.RData

clean:
	RM -f -r "$(TEMP_DIR)"
	RM -f -r "$(OUTPUT_DIR)"
	RM -f -r "$(AUDIT_DIR)"
	MKDIR "$(OUTPUT_DIR)"
	MKDIR "$(TEMP_DIR)"
	MKDIR "$(AUDIT_DIR)"
	

###### DATA PREPARATION ######

# unzip data
$(TEMP_DIR)/unzipped.RData : unzip_csv.R
	R --vanilla --args "" < "unzip_csv.R"

# prepare advertising data
$(TEMP_DIR)/advertising.csv: get_advertising.R
	R --vanilla --args "" < "get_advertising.R"

# clean exchange rates and CPIs
$(TEMP_DIR)/exch_cpi.RData : exch_cpi.R
	R --vanilla --args "" < "exch_cpi.R"

# prepare GDP per capita data
$(TEMP_DIR)/gdppercap.RData : gdp.R
	R --vanilla --args "" < "gdp.R"

# re-categorize categories
$(TEMP_DIR)/categorized.RData : categorize.R $(TEMP_DIR)/unzipped.RData
	R --vanilla --args "" < "categorize.R"

# extract attribute levels per category
$(TEMP_DIR)/extract_attributes.RData : extract_attributes.R $(TEMP_DIR)/categorized.RData
	R --vanilla --args "" < "extract_attributes.R"

# define attribute levels per category
$(TEMP_DIR)/attributes.RData : set_attributes.R $(TEMP_DIR)/extract_attributes.RData
	R --vanilla --args "" < "set_attributes.R"

# calculate uniqueness metrics and lagged sales measure for re-weighing SKUs
$(TEMP_DIR)/uniqueness_and_lagsales.RData : uniqueness_and_lagsales.R proc_functions.R $(TEMP_DIR)/categorized.RData $(TEMP_DIR)/attributes.RData
	R --vanilla --args "" < "uniqueness_and_lagsales.R"

# select periods in which category sales are meaningful; select brands vs. grouped "other" brands
$(TEMP_DIR)/select.RData : select.R $(TEMP_DIR)/uniqueness_and_lagsales.RData
	R --vanilla --args "" < "select.R"

# prepare brand-level data set (e.g., remove/interpolate missings)
$(TEMP_DIR)/brand_metrics.RData: brand_metrics.R $(TEMP_DIR)/exch_cpi.RData $(TEMP_DIR)/select.RData $(TEMP_DIR)/uniqueness_and_lagsales.RData $(TEMP_DIR)/gdppercap.RData $(TEMP_DIR)/advertising.csv
	R --vanilla --args "" < "brand_metrics.R"

# do final time selection
$(OUTPUT_DIR)/datasets.txt: time_select.R $(TEMP_DIR)/brand_metrics.RData
	R --vanilla --args "" < "time_select.R"
	
# --> audit: create category sales plots
$(TEMP_DIR)/audit_rawplot.txt: audit_rawplot.R $(TEMP_DIR)/categorized.RData
	R --vanilla --args "" < "audit_rawplot.R"

# --> audit: create plots of all markets
$(TEMP_DIR)/audit_markets.txt: audit_markets.R $(OUTPUT_DIR)/datasets.txt
	R --vanilla --args "" < "audit_markets.R"

# --> audit: create plots of brands
$(TEMP_DIR)/audit_brands.txt: audit_brands.R $(OUTPUT_DIR)/datasets.txt
	R --vanilla --args "" < "audit_brands.R"

# --> audit: create plots of brands
$(TEMP_DIR)/audit_attributes.txt: audit_attributes.R $(OUTPUT_DIR)/datasets.txt
	R --vanilla --args "" < "audit_attributes.R"

# --> audit: create plots of brands
$(TEMP_DIR)/audit_advertising.txt: audit_advertising.R #(TEMP_DIR)/advertising.csv
	R --vanilla --args "" < "audit_advertising.R"
