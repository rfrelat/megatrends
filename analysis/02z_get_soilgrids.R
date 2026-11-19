# Script to get the Soilgrids
#   https://soilgrids.org/
#
# input:
#   raw-data/soilgrids/XX.tif # all files are downloaded in the script
# output:
#   indicators_csv/MAILLEXkm_SOILGRIDS.csv
#   indicators_csv/COMMUNE_SOILGRIDS.csv
#   figure/SOIL_XX_MAILLEXkm.png
#   figure/SOIL_XX_COMMUNE.png

# 1. Set-up: libraries and working directory -----------------------------------

devtools::load_all()
library(sf)
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "soilgrids")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

convlab <- c(
  "bdod" = "SOIL_BULK_DENSITY_cg_per_cm3",
  "clay" = "SOIL_CLAY_CONTENT_g_per_kg",
  "wv0033" = "SOIL_WATER_CONTENT_33kPA",
  "soc" = "SOIL_ORGANIC_CARBON_STOCK_dg_per_kg",
  "nitrogen" = "SOIL_NITROGEN_CONTENT_cg_per_kg"
)

depth <- "0-5cm_mean"

# 2. Download Soildgrid data ------------------------------------

# Create data folder if necessary
if (!dir.exists(data_folder)) {
  dir.create(
    path = data_folder,
    showWarnings = FALSE,
    recursive = TRUE
  )
}

# Set French boundaries
fr <- sf::st_read(file.path(ref_folder, "france_4326.gpkg"))
fr_box <- sf::st_bbox(fr) |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

# Soilgrids data (114Mb)
url_sg <- "https://maps.isric.org/mapserv?map=/map/VV.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=VV_DD&FORMAT=image/tiff&SUBSET=long(-5,10)&SUBSET=lat(40,55)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
selected_sg <- names(convlab)
for (i in selected_sg) {
  file_i <- paste0(i, "_", depth, ".tif")
  fileout <- file.path(data_folder, file_i)
  if (!file.exists(fileout)) {
    # update url
    urli <- gsub("VV", i, gsub("DD", depth, url_sg))
    # download the file
    download.file(urli, fileout, mode = "wb")
  }
}

# 3. Overlay and calculate statistics -----------------------------
list_sg <- list.files(data_folder)
lab <- gsub("(.*)_0-5cm_mean\\.tif$", "\\1", list_sg)
newlab <- convlab[lab]
soilgd <- rast(file.path(data_folder, list_sg))
names(soilg) <- lab

for (i in scales) {
  print(i)
  if (i == "commune") {
    shp <- vect(file.path(ref_folder, "commune_4326.gpkg"))
    shp$id <- shp$INSEE_COM
  } else {
    shp <- vect(file.path(ref_folder, paste0(i, "_4326.gpkg")))
    shp$id <- shp$cd_sig
  }

  labi <- gsub("KM$", "km", toupper(gsub("_", "", i)))

  soili <- exactextractr::exact_extract(
    soilgd,
    sf::st_as_sf(shp),
    fun = "mean"
  )

  shp[, newlab] <- round(soili, 3)

  for (j in newlab) {
    fj <- paste0(j, "_", labi, ".png")

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
      breaks = 6
    )
    dev.off()
  }

  # export
  out_fi <- paste0(gsub("S", "", labi), "_SOILGRIDS.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
