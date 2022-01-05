# Cross-National Differences in Market Response

Replication package for [__Cross-National Differences in Market Response: Line-Length, Price, and Distribution Elasticities in Fourteen Indo-Pacific Rim Economies__](https://doi.org/10.1177%2F00222437211058102), *Journal of Marketing Research*, forthcoming. https://doi.org/10.1177%2F00222437211058102.

by Hannes Datta, Harald J. van Heerde, Marnik G. Dekimpe and Jan-Benedict E.M. Steenkamp

## Abstract

Our field’s knowledge of marketing-mix elasticities is largely restricted to developed countries in the North-Atlantic region, even though other parts of the world—especially the Indo-Pacific Rim region—have become economic powerhouses. To better allocate marketing budgets, firms need to have information about marketing-mix elasticities for countries outside the North-Atlantic region. 

We use data covering over 1,600 brands from 14 product categories collected in 7 developed and 7 emerging Indo-Pacific Rim countries across more than 10 years to estimate marketing elasticities for line length, price, and distribution, and examine which brand, category, and country factors influence these elasticities. 

Averaged across brands, categories, and countries, line-length elasticity is .459, price elasticity is -.422, and distribution elasticity is .368, but with substantial variation across brands, categories, and countries. Contrary to what has been suggested, we find no systematic differences in marketing responsiveness between emerging and developed economies. Instead, the key country-level factor driving elasticities is societal stratification, with Hofstede’s measure of power inequality (power distance) as its cultural manifestation and income inequality as its economic manifestation. As the effects of virtually all brand, category, and country factors differ across the three marketing-mix instruments, the field needs new theorizing that is contingent on the marketing-mix instrument studied.

## Overview of this Replication Package

This replication package consists of six elements, as layed out in the replication guidelines from Tilburg School of Economics and Management (TiSEM). 

1. Metadata and data collection
   - a) File version history available on GitHub
   - b) Package has been compiled by Hannes Datta; package copies are stored at Dataverse, and are available from each of the four coauthors
   - c) No ethical review for this project has been carried out
   - d) Role of co-authors: Hannes Datta (devising and organizing the project, data collection, data analysis, article writing), Harald J. van Heerde (data collection, data analysis, article writing), Marnik G. Dekimpe (data collection, data analysis, article writing), Jan-Benedict E.M. Steenkamp (data collection, data analysis, article writing).
   - e) Data collection: GfK datasets supplied by GfK Singapore (main datasets); supplementary data compiled by co-authors
   - f) Reliance on external data sources: If applicable, sources noted in the paper
   - g) No external finances/grants obtained for this projects. Advertising data have been bought with (internal) University funds.
   - h) Date of acceptance: 21 October 2021, https://doi.org/10.1177%2F00222437211058102
2. Raw data
   - The project relies largely on (confidential) data supplied by GfK singapore, governed by an NDA between GfK and the coauthors. The data is stored in `data/gfk_2012` and `data/gfk_2015` (marking two delivery batches). The data is not available to the general public, and only available for replication purposes in the event of an investigation into alleged research misconduct (see also Netherlands Code of Conduct for Research Integrity and TiSEM's replication policy).
   - Other data files in this project are obtained from websites, APIs, and official data sources.
   - The raw data directory contains a list with all files in the package, along with their dates of creation and file hashes.
3. Material
   - No supplementary material, other than computer code in this repository and data files, are needed to replicate the project.
4. Programming code
   - The programming code 
   - 
- ADD NDA
- create file version tree

### Metadata and data collection


Repository overview

```
├── README.md (this documentation)
├── data (raw data)
├── code
│   ├── analysis       <- analysis: sales response models
│   ├── derived        <- data preparation
│   ├── post-analysis  <- second-stage analysis
│   └── simulations    <- simulation studies (copulas, parameter recovery)
```

__Each module contains a makefile in a subdirectory `\code`__, which can be run by navigating to the directory, and typing `make`.


## Running instructions


1. Install software

- install Git, available at https://git-scm.com/download/win
- install GNU Make, available at http://gnuwin32.sourceforge.net/packages/make.htm
- install R, and make R available via the path settings
    
2. Checkout repository

Checkout this Git repository: `git clone https://github.com/hannesdatta/gfk_singapore.git`

3. Obtain raw data

The directory structure in the repository contains a `data` folder, which is still empty. The raw data files for this project are largely protected by an NDA between the coauthors and GfK, the main supplier of the data.

For replication purposes, the data has been archived at [...], but this data is only available for download for authorized personell.

4. Install required R packages

Run `install_packages.R` in the repository's root directory.

5. Run `make` to build each of the projects' main modules.


