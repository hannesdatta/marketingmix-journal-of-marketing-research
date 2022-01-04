# Makefile for the \simulations module
# requires GNU Make

TEMP_DIR = ../temp
OUTPUT_DIR = ../output
EXT_DIR = ../externals

all: $(OUTPUT_DIR)/gaussian-copulas.html $(OUTPUT_DIR)/latent-instruments.pdf

$(OUTPUT_DIR)/latent-instruments.pdf: latent-instruments.Rmd liv_llik.R
	R -e "rmarkdown::render('latent-instruments.Rmd', output_file = '../output/latent-instruments.pdf')"

$(OUTPUT_DIR)/gaussian-copulas.html: gaussian-copulas.Rmd 
	R -e "rmarkdown::render('gaussian-copulas.Rmd', output_file = '../output/gaussian-copulas.html')"