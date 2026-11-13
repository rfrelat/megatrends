# Script to get the Grassland Mowing Events from Copernicus
#   https://land.copernicus.eu/en/products/high-resolution-layer-grasslands?tab=grassland_mowing_events
#
# input:
#   copernicus/CLMS_HRLVLCC_GRAME_FR_2020.tif # 10m resolution
# output:
#   indicators_csv/MAILLEXkm_COPERNICUS_2020.csv
#   indicators_csv/COMMUNE_COPERNICUS_2020.csv
#   figure/MOWING_EVENTS_MAILLEXkm.png
#   figure/MOWING_EVENTS_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "copernicus")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load fragmentation data ------------------------------------
filemos <- file.path(data_folder, "CLMS_HRLVLCC_GRAME_FR_2020.tif")
if (!file.exists(filemos)) {
  # combine tiles if not done already
  tiles <- list.files(data_folder, "tif$", full.names = TRUE)
  rlist <- lapply(tiles, rast)
  mos <- mosaic(sprc(rlist))

  # # plot(mos)
  # # france <- vect(file.path(ref_folder, "france_4326.gpkg"))
  # # plot(france, add = TRUE)

  writeRaster(mos, filemos)
}

# Grassland Mowing Events
grass <- rast(filemos)
NAflag(grass) <- 253

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

  grassi <- exactextractr::exact_extract(
    grass,
    sf::st_as_sf(shp),
    fun = "mean"
  )

  shp$MOWING_EVENTS_2020 <- round(grassi, 2)

  png(
    file = file.path(fig_folder, paste0("MOWING_EVENTS_", labi, ".png")),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "MOWING_EVENTS_2020",
    border = NA,
    main = paste("MOWING_EVENTS_2020 -", i),
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_COPERNICUS_2020.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
