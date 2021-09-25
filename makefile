# Makefile for the entire project
# requires GNU Make

###### BUILD COMMANDS ######

all: postanalysis simulations

.PHONY: all derived analysis postanalysis

derived:
	$(MAKE) -C derived/code

analysis: derived
	$(MAKE) -C analysis/code

postanalysis: analysis
	$(MAKE) -C post-analysis/code

simulation: simulation
	$(MAKE) -C simulation/code

