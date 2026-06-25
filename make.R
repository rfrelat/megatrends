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

# 4 steps:

# 1. Check, complete and verify reference list
source(here::here("analysis", "01a_conv_ref.R"))
# additional scripts
# to get synonyms for commune
source(here::here("analysis", "01b_get_synonyms.R"))
# to intersect commune and mailles
source(here::here("analysis", "01c_cross_ref.R"))

# 2. Get megatrends per data sources
# CARTOBIO: percentage of organic fields
source(here::here("analysis", "02a_get_bio.R"))
# WDPA: percentage of protected areas
source(here::here("analysis", "02b_get_wdpa.R"))
# HVE: High environemental value subsidies
source(here::here("analysis", "02c_get_hve.R"))
# Telepac: amount of green subsidies
source(here::here("analysis", "02d_get_telepac.R"))
# Pesticide: Rigal and Perrot 2025
source(here::here("analysis", "02e_get_pesticide.R"))
# Nitrate: EauFrance 2024: very patchy
# source(here::here("analysis", "02fbis_get_NO3_eaufrance.R"))
# Nitrate: EauPotable 2025
source(here::here("analysis", "02f_get_NO3_eaupotable.R"))
# RPG 2023: ag practices
source(here::here("analysis", "02g_get_rpg.R"))
# Agreste 2020: Recensement agricole
source(here::here("analysis", "02h_get_agreste.R"))
# Crop rotation
source(here::here("analysis", "02i_get_croptation.R"))
#
# to be completed ...

# 3. Merge everything
source(here::here("analysis", "03_merge_indicators.R"))

# 4. Create overview
quarto::quarto_render("analysis/04_political_stringency.qmd")

# 5. Run the shiny app
app_path <- here::here("app")
shiny::runApp(app_path, display.mode = "normal")

file_app <- rsconnect::listDeploymentFiles(app_path)
file_app <- file_app[!file_app %in% "miniapp.R"]
rsconnect::deployApp(
  appDir = app_path,
  appFiles = file_app,
  appName = "Motiver_megatrends",
  appTitle = "Motiver Megatrends"
)
# 15Mb
