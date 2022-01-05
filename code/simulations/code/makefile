# Makefile for the \simulations module
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output
EXT_DIR = ../externals

all: $(OUTPUT_DIR)/gaussian-copulas.html 

$(OUTPUT_DIR)/gaussian-copulas.html: gaussian-copulas.Rmd
	mkdir ../output
	R -e "rmarkdown::render('gaussian-copulas.Rmd', output_file = '../output/gaussian-copulas.html')"