# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

all: $(OUTPUT_DIR)/final_analysis.Rmd

###### Build parts ######


# Prepare GDP data


$(OUTPUT_DIR)/final_analysis.Rmd: final_analysis.Rmd proc_auxilary.R proc_rename.R preclean.R pre
	"$(RBIN64)Rscript.exe" -e "rmarkdown::render('final_analysis.Rmd', output_file = '../output/final_analysis.html')"
