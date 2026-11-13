# Script to get the soil information from JRC-ESDAC
#  -Assessment of soil erosion by water at 100m resolution using the
#   Revised Universal Soil Loss Equation (RUSLE) model and based on
#   rainfall, soil, topography, land use and management from the year 2016.
#   https://esdac.jrc.ec.europa.eu/content/soil-erosion-water-rusle2015
#  -100m resolution map of cover crops using disaggregation model based on
#   satellite data (Sentinel-1) and survey data for the year 2016
#   https://esdac.jrc.ec.europa.eu/content/cover-crops-accross-europe
#
# input:
#   fragmentation/SEFF_2018_10m.tif # 100m resolution
#   fragmentation/meff_2018_EEA38plusUK_1km_32bit_float_final.tif #1km resolution
# output:
#   indicators_csv/MAILLEXkm_ESDAC.csv
#   indicators_csv/COMMUNE_ESDAC.csv
#   figure/ESDAC_XX_MAILLEXkm.png
#   figure/ESDAC_XX_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "esdac")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load fragmentation data ------------------------------------
# soil erosion by water
rusle <- rast(file.path(data_folder, "NEW_RUSLE2016.tif"))

# cropcover
crop <- rast(file.path(data_folder, "CC_Median.tif"))
# all in EPSG: 3035

# 3. Overlay and calculate statistics -----------------------------
for (i in scales) {
  print(i)
  if (i == "commune") {
    shp <- vect(file.path(ref_folder, "commune_3035.gpkg"))
    shp$id <- shp$INSEE_COM
  } else {
    shp <- vect(file.path(ref_folder, paste0(i, "_3035.gpkg")))
    shp$id <- shp$cd_sig
  }

  labi <- gsub("KM$", "km", toupper(gsub("_", "", i)))

  rusi <- exactextractr::exact_extract(
    rusle,
    sf::st_as_sf(shp),
    fun = "mean"
  ) |>
    suppressWarnings()

  shp$EROSION_t_PER_HA_2016 <- round(rusi, 3)

  crpi <- exactextractr::exact_extract(
    crop,
    sf::st_as_sf(shp),
    fun = "mean"
  ) |>
    suppressWarnings()

  shp$CROP_COVER_PCT_2016 <- round(crpi / 100, 3)

  var <- c(
    "EROSION_t_PER_HA_2016",
    "CROP_COVER_PCT_2016"
  )
  for (j in var) {
    fj <- paste0("ESDAC_", j, "_", labi, ".png")

    png(
      file = file.path(fig_folder, fj),
      width = 1200,
      height = 1000,
      res = 200
    )
    plot(
      shp,
      y = j,
      border = NA,
      main = paste(j, "-", i),
      breaks = 6,
      breakby = "cases"
    )
    dev.off()
  }

  # export
  out_fi <- paste0(gsub("S", "", labi), "_ESDAC.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
