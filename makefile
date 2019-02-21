# Makefile for \derived of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

include C:/make_research

###### BUILD COMMANDS ######

# full build
all: postanalysis

derived:
	cd derived/code && $(MAKE)

analysis: derived
	cd analysis/code && $(MAKE)

postanalysis: analysis
	cd post-analysis/code &&  $(MAKE)

