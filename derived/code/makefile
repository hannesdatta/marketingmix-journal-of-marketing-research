# Makefile for \derived of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

#.PHONY : $(TEMP_DIR)/%.RData

# full build
build: $(OUTPUT_DIR)/datasets.RData

# audit
auditplots: audit_markets
audit: audit_attrib audit_catsales_by audit_markets

# building parts
unzip: $(TEMP_DIR)/unzipped.RData
categorize: $(TEMP_DIR)/categorized.RData

###### DATA PREPARATION ######

# unzip data
$(TEMP_DIR)/unzipped.RData : unzip_csv.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "unzip_csv.R"

# prepare Chinese advertising data
$(TEMP_DIR)/adv_china.RData : advertising.R
	"$(RBIN32)Rterm.exe" --vanilla --args "" < "advertising.R"

# clean exchange rates and CPIs
$(TEMP_DIR)/exch_cpi.RData : exch_cpi.R
	"$(RBIN32)Rterm.exe" --vanilla --args "" < "exch_cpi.R"

# prepare GDP per capita data
$(TEMP_DIR)/gdppercap.RData : gdp.R
	"$(RBIN32)Rterm.exe" --vanilla --args "" < "gdp.R"

# re-categorize categories
$(TEMP_DIR)/categorized.RData : categorize.R $(TEMP_DIR)/unzipped.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "categorize.R"

# define attribute levels per category
$(TEMP_DIR)/attributes.RData : set_attributes.R $(TEMP_DIR)/categorized.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "set_attributes.R"

# --> audit: create category sales plots
audit_attrib: audit_attributes_txt.R $(TEMP_DIR)/attributes.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "audit_attributes_txt.R"

# --> audit: create category sales plots
audit_catsales_by: audit_categorysales_by.R $(TEMP_DIR)/categorized.RData $(TEMP_DIR)/attributes.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "audit_categorysales_by.R"

# calculate uniqueness metrics and lagged sales measure for re-weighing SKUs
$(TEMP_DIR)/uniqueness_and_lagsales.RData : uniqueness_and_lagsales.R proc_functions.R $(TEMP_DIR)/categorized.RData $(TEMP_DIR)/attributes.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "uniqueness_and_lagsales.R"

# select periods in which category sales are meaningful; select brands vs. grouped "other" brands
$(TEMP_DIR)/select_periods_and_brands.RData : select_periods_and_brands.R $(TEMP_DIR)/uniqueness_and_lagsales.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "select_periods_and_brands.R"

# prepare brand-level data set (e.g., remove/interpolate missings)
$(TEMP_DIR)/brand_metrics.RData : brand_metrics.R $(TEMP_DIR)/exch_cpi.RData $(TEMP_DIR)/select_periods_and_brands.RData $(TEMP_DIR)/uniqueness_and_lagsales.RData $(TEMP_DIR)/attributes.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "brand_metrics.R"

# prepare category-level data, package all data sets in one .RData file
$(OUTPUT_DIR)/datasets.RData : category_metrics.R $(TEMP_DIR)/brand_metrics.RData $(TEMP_DIR)/gdppercap.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "category_metrics.R"

# --> audit: create plots of all markets
audit_markets: audit_marketplots.R $(OUTPUT_DIR)/datasets.RData
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "audit_marketplots.R"
