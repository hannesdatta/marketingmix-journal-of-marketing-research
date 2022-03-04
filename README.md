# Cross-National Differences in Market Response: Line-Length, Price, and Distribution Elasticities in Fourteen Indo-Pacific Rim Economies

Replication package for [__Cross-National Differences in Market Response: Line-Length, Price, and Distribution Elasticities in Fourteen Indo-Pacific Rim Economies__](https://doi.org/10.1177%2F00222437211058102), *Journal of Marketing Research*, February 2022.

by Hannes Datta, Harald J. van Heerde, Marnik G. Dekimpe and Jan-Benedict E.M. Steenkamp

Links:
- Paper at https://doi.org/10.1177%2F00222437211058102.
- Replication files
    - archived version available at https://doi.org/10.34894/EVPJTY
    - browsable version available at https://github.com/hannesdatta/marketingmix-journal-of-marketing-research
- Publicly available data does __not__ contain any of the raw data, which is accessible only for replication purposes (see below for more details)

## Abstract

Our field’s knowledge of marketing-mix elasticities is largely restricted to developed countries in the North-Atlantic region, even though other parts of the world—especially the Indo-Pacific Rim region—have become economic powerhouses. To better allocate marketing budgets, firms need to have information about marketing-mix elasticities for countries outside the North-Atlantic region. 

We use data covering over 1,600 brands from 14 product categories collected in 7 developed and 7 emerging Indo-Pacific Rim countries across more than 10 years to estimate marketing elasticities for line length, price, and distribution, and examine which brand, category, and country factors influence these elasticities. 

Averaged across brands, categories, and countries, line-length elasticity is .459, price elasticity is -.422, and distribution elasticity is .368, but with substantial variation across brands, categories, and countries. Contrary to what has been suggested, we find no systematic differences in marketing responsiveness between emerging and developed economies. Instead, the key country-level factor driving elasticities is societal stratification, with Hofstede’s measure of power inequality (power distance) as its cultural manifestation and income inequality as its economic manifestation. As the effects of virtually all brand, category, and country factors differ across the three marketing-mix instruments, the field needs new theorizing that is contingent on the marketing-mix instrument studied.

## Overview of this Replication Package

This replication package is based on the [replication guidelines]( https://www.tilburguniversity.edu/research/economics-and-management/replication-package) from Tilburg School of Economics and Management (TiSEM). 

### 1) Metadata and data collection

a) File logs available in `docs/files_in_repository.csv`. All files have been created and edited by Hannes Datta, except (many) of the raw data files (see below).

b) Access was available to all coauthors; final replication package compiled by Hannes Datta. Copies of the replication package are stored at Dataverse and are available from each of the four coauthors.

c) No ethical review for this project was conducted nor required by the institutional policy when starting this project.

d) Role of coauthors: Hannes Datta (devising and organizing the project, data collection, data analysis, article writing), Harald J. van Heerde (data collection, data analysis, article writing), Marnik G. Dekimpe (data collection, data analysis, article writing), Jan-Benedict E.M. Steenkamp (data collection, data analysis, article writing).

e) Data collection: GfK datasets supplied by GfK Singapore (main datasets); supplementary data compiled by coauthors.

f) Reliance on external data sources: If applicable, sources noted in the paper.

g) Research data from GfK has been provided in kind. Advertising data have been bought with research funds from Tilburg University (Marnik G. Dekimpe) and Massey University (Harald J. van Heerde). No other funds were acquired for this project.

h) Date of acceptance: 18 October 2021, https://doi.org/10.1177%2F00222437211058102 (online first on 21 October 2021).

### 2) Raw data

- The project relies mainly on (confidential) data supplied by GfK Singapore, governed by an NDA between GfK and the coauthors. The data is stored in `data/gfk2012` and `data/gfk2015` (marking two delivery batches). Advertising data was bought and kept confidential (`data/advertising`). __These datasets are not available to the general public but only available for replication purposes in the event of an investigation into alleged research misconduct__ (see also Netherlands Code of Conduct for Research Integrity and TiSEM's replication policy). The files are stored at Tilburg University (`\\stafffiles.campus.uvt.nl\files\shared\research\MarketingMix-Pacific-Rim-jmr`), and copies are available with all coauthors. 
- Other data files in this project are obtained from websites, APIs, and official data sources.
- The `docs/files_in_repository.csv` file contains a list with all files in the package, along with their dates of creation, etc. 

### 3) Material

The folder `data/other` contains supplementary material (e.g., illustrative calculations) used throughout the paper (e.g., the population in the US, relative to the world population). This project can be entirely replicated using computer code (`code`) and the remaining raw data files (`data`).

### 4) Programming code
- The programming code is fully available; see subfolder `code\`. 
- Submodules are `code\derived` (for converting the raw data to data sets for analysis), `code\anlaysis` (for running the sales response models), `code\post-analysis` (for running seecond-stage regressions), and `code\simulations` for simulation studies. 
- The order of execution in each submodule is made explicit in `makefile`. 


```
├── README.md (this documentation)
├── data (raw data - only available for replication purposes given the NDA between coauthors and data supplier)
├── code
│   ├── analysis       <- analysis: sales response models
│   ├── derived        <- data preparation
│   ├── post-analysis  <- second-stage analysis
│   └── simulations    <- simulation studies (copulas, parameter recovery)
```

### 5) Processed database(s)

- Any processed information is available in the subfolders of each main module - derived, analysis, post-analysis, and simulations. 
- Processed files include temporary files (`temp/`), auditing files (`audit/`), and output files (`output/`). All these files are exclusively produced based on code. 

### 6) Accepted or published manuscript or publication

Published manuscript available at https://doi.org/10.1177%2F00222437211058102.

## Replication Instructions

__Each module contains a makefile in a subdirectory `\code`__, which can be run by navigating to the directory, and typing `make`.

1. Install software

- install Git, available at https://git-scm.com/download/win
- install GNU Make, available at http://gnuwin32.sourceforge.net/packages/make.htm
- install R, and make R available via the path settings
    
2. Checkout repository

Use obtained source code or check out from Git(Hub).

3. Obtain raw data

The directory structure in the repository contains a `data` folder, which could be empty if not received directly from the coauthors (for confidentiality reasons). When replicating the results of this project, the raw data files need to be inserted into this folder.

4. Install required R packages

Run `install_packages.R` in `code/tools` (using the main project directory as the working directory).

5. Run `make` to build each of the projects' main modules.

## Documentation

The documentation for this project has been build using the tools available in `code\tools`. The data pipeline can be executed using `make release`.
