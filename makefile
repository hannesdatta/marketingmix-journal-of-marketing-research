# Makefile for \derived of the Gfk Singapore project
# by Hannes Datta
# requires GNU Make

###### BUILD COMMANDS ######

# full build
all: postanalysis

.PHONY: all derived analysis postanalysis

derived:
	$(MAKE) -C derived/code

analysis: derived
	$(MAKE) -C analysis/code

postanalysis: analysis
	$(MAKE) -C post-analysis/code

