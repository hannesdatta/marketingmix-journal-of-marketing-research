# Makefile for \draft of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

.PHONY : all

###### DATA ANALYSIS ######

clean:
	del "..//output//*"

# analyze data
run : results.R template.Rnw load.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "results.R"

all:	clean run

fit : fit.R load.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "fit.R"
