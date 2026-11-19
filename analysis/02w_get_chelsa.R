# Script to get the Chelsa climate data
#   https://www.chelsa-climate.org/
#   https://www.chelsa-climate.org/datasets/chelsa_bioclim
#   https://www.chelsa-climate.org/datasets/chelsa_annual
#
# input:
#   raw-data/chelsa/XX.tif # all files are downloaded in the script
# output:
#   indicators_csv/MAILLEXkm_CHELSA.csv
#   indicators_csv/COMMUNE_CHELSA.csv
#   figure/CHELSA_XX_MAILLEXkm.png
#   figure/CHELSA_XX_COMMUNE.png

# 1. Set-up: libraries and working directory -----------------------------------

devtools::load_all()
library(sf)
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "chelsa")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

convlab <- c(
  "bio01_1981-2010" = "CHELSA_BIO01_ANNUAL_TEMP_DC",
  "bio10_1981-2010" = "CHELSA_BIO10_SUMMER_TEMP_DC",
  "bio12_1981-2010" = "CHELSA_BIO12_ANNUAL_PREC_MM",
  "bio18_1981-2010" = "CHELSA_BIO18_SUMMER_PREC_MM",
  "fcf_1981-2010" = "CHELSA_FCF_FROST_FREQ",
  "spei12_1981-2010" = "CHELSA_SPEI12_WATER_BALANCE_1981_2010",
  "spei12_2009-2018" = "CHELSA_SPEI12_WATER_BALANCE_2009_2018"
)

# 2. Download CHELSA data ------------------------------------

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

# Chelsa bioclim data (15Mb)
# https://www.chelsa-climate.org/datasets/chelsa_bioclim
# you can click and download individual files
# e.g. "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/bioclim/bio01/1981-2010/CHELSA_bio01_1981-2010_V.2.1.tif"
# or automate it and crop the region of interest

url_bioclim <- "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/bioclim/XXX/1981-2010/CHELSA_XXX_1981-2010_V.2.1.tif"
selected <- c("bio01", "bio10", "bio12", "bio18", "fcf")

for (i in selected) {
  file_i <- paste0("CHELSA_", i, "_1981-2010_Fr.tif")
  fileout <- file.path(data_folder, file_i)
  if (!file.exists(fileout)) {
    # add vsicurl to download only subset
    urli <- paste0("/vsicurl/", gsub("XXX", i, url_bioclim))
    # is much faster with 'vsicurl' (no need to download all data)
    chelsa_i <- terra::rast(urli)
    # crop to the extent of intest
    crop_i <- terra::crop(chelsa_i, fr_box)
    # export
    writeRaster(crop_i, fileout)
  }
}

# Chelsa drought indices
# https://www.chelsa-climate.org/datasets/chelsa_annual
yr <- 1981:2010 # 2010
file_i <- paste0("CHELSA_spei12_", min(yr), "-", max(yr), "_Fr.tif")
fileout <- file.path(data_folder, file_i)

url_drought <- "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/annual/spei12/YYYY/CHELSA_spei12_MM_YYYY_V.2.1.tif"

if (!file.exists(fileout)) {
  mo <- paste0(ifelse(1:12 < 10, "0", ""), 1:12)
  # consider all possible time steps
  timestep <- expand.grid(mo = mo, yr = yr)
  # get the correct urls for all timesteps
  urls <- sapply(1:nrow(timestep), function(i) {
    gsub("MM", timestep$mo[i], gsub("YYYY", timestep$yr[i], url_drought))
  })
  # add vsicurl to download only subset
  urls <- paste0("/vsicurl/", urls)
  # is much faster with 'vsicurl'
  chelsa_i <- terra::rast(urls)
  # crop to the extent of intest
  crop_i <- terra::crop(chelsa_i, fr_box)
  # compute the average
  mean_i <- mean(crop_i, na.rm = TRUE)
  # export
  writeRaster(mean_i, fileout)
}

# 3. Overlay and calculate statistics -----------------------------
list_chelsa <- list.files(data_folder)
lab <- gsub("^CHELSA_(.*)_Fr\\.tif$", "\\1", list_chelsa)
newlab <- convlab[lab]
chelsa <- rast(file.path(data_folder, list_chelsa))
names(chelsa) <- lab

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

  chelsi <- exactextractr::exact_extract(
    chelsa,
    sf::st_as_sf(shp),
    fun = "mean"
  )

  shp[, newlab] <- round(chelsi, 3)

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
  out_fi <- paste0(gsub("S", "", labi), "_CHELSA.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
