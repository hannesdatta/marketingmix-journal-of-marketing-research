# Makefile for \analysis of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include ../../tools/common

TEMP_DIR = ../temp
OUTPUT_DIR = ../output


###### BUILD COMMANDS ######

.PHONY : run

###### DATA ANALYSIS ######

# analyze data
run : analysis.R proc_analysis.R proc_sur.R
	"$(RBIN64)Rterm.exe" --vanilla --args "" < "analysis.R"
