# Megatrends : Research compendium for extracting megatrends maps in France 

Code and data used to extracting megatrends maps in France 


## General

This repository is structured as follow:

- :file_folder: &nbsp;`analysis/`: contains R scripts to extract megatrends
- :file_folder: &nbsp;`data/`: contains raw and derived data (stored in Nextcloud folder);
- :file_folder: &nbsp;`figure/`: contains reprensentation of the extracted megatrends;
- :file_folder: &nbsp;`progress/`: contains documents to keep track of discussions and decisions;
- :file_folder: &nbsp;`R/`: contains home-made R functions that smooth the megatrend extraction;



## Usage

The analysis is divided in three sequential steps:  

1. Check and verify the reference spatial data (commune and maille)
2. Get the indicators per database at the scale of commune and 10km-grid
3. Merge all the indicators

The analysis takes 5 hours to run on a computing server. 

These two steps will be run automatically when run this command in R/RStudio: 

```r
source("make.R")
```

The file `make.R` can be run to recompute all indicators. As the analysis is carried out at the scale of France, it takes some hours to run.
