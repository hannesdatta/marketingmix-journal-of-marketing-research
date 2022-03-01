# Makefile for the entire project
# requires GNU Make

# Main project pipeline

all: postanalysis simulations

.PHONY: all derived analysis postanalysis

derived:
	$(MAKE) -C code/derived/code

analysis: derived
	$(MAKE) -C code/analysis/code

postanalysis: analysis
	$(MAKE) -C code/post-analysis/code

simulations: 
	$(MAKE) -C code/simulations/code

# Files required to build public release

release: docs/files_in_repository.csv public/log.txt

docs/files_in_repository.csv: code/tools/create_file_overview.R
	Rscript code/tools/create_file_overview.R

public/log.txt: docs/files_in_repository.csv code/tools/create_public_version.py
	python code/tools/create_public_version.py