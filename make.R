## Harmonize city names and get INSEE code

## Make sure needed packages are installed ---------
# make sure all needed packages are installed
if (!requireNamespace("here", quietly = TRUE)) {
  # to avoid issues of relative file path
  install.packages("here")
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  # to load excel files
  install.packages("readxl")
}
if (!requireNamespace("terra", quietly = TRUE)) {
  # to load the shapefile
  install.packages("terra")
}
if (!requireNamespace("stringdist", quietly = TRUE)) {
  # to compute distance between strings
  install.packages("stringdist")
}

devtools::load_all()

## Run Project --------------------------------------------

# 2 steps:

# 1. Check, complete and verify reference list
# source(here::here("analysis", "01_conv_ref.R"))

# 2. Get megatrends per data sources
# CARTOBIO: percentage of organic fields
source(here::here("analysis", "02a_get_bio.R"))
# WDPA: percentage of protected areas
source(here::here("analysis", "02b_get_wdpa.R"))

# to be completed ...

# 3. Merge everything
#
# to be completed
