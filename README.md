# Megatrends : Research compendium for extracting megatrends maps in France 

Code and data used to extract megatrends maps in France 


## General

This repository is structured as follow:

- :file_folder: &nbsp;`analysis/`: contains R scripts to extract megatrends, 
- :file_folder: &nbsp;`app/`: contains the shiny app to visualize the metrics extracted; 
- :file_folder: &nbsp;`data/`: contains raw and derived data (stored in Nextcloud folder);
- :file_folder: &nbsp;`docs/`: contains the online visualization of the megatrends;
- :file_folder: &nbsp;`figure/`: contains the static maps of the extracted metrics;
- :file_folder: &nbsp;`progress/`: contains documents to keep track of discussions and decisions;
- :file_folder: &nbsp;`R/`: contains home-made R functions that smooth the megatrend extraction;



## Usage

The analysis is divided in three sequential steps:  

1. Check and verify the reference spatial data (commune and maille)
2. Get the indicators per database at the scale of commune and 10km-grid
3. Merge all the indicators

The analysis takes some hours to run on a computing server. 

These two steps will be run automatically when run this command in R/RStudio: 

```r
source("make.R")
```

The file `make.R` can be run to recompute all indicators. As the analysis is carried out at the scale of France, it takes multiple hours to run.


## Overview

You can have an overview of the calculated indicators:  

- through a Shiny App: <https://rfrelat-cesab.shinyapps.io/Motiver_megatrends/>  
- with a focus on political stringency indicators: <https://rfrelat.github.io/megatrends/>

<!-- 
```r
shiny::runApp("/home/rfrelat/Documents/megatrends/app")
``` 
-->