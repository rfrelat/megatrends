# Script to get slopes from Geomorph90m
#
# input:
#   geomorph90 from Amatulli et al. 2020 https://doi.org/10.1038/s41597-020-0479-6
#     https://opentopography.s3.sdsc.edu/minio/dataspace/OTDS.012020.4326.1/
#   geomorph/slope_90M_France.tif
#
# output:
#   indicators_csv/MAILLEXkm_SLOPE.csv
#   indicators_csv/COMMUNE_SLOPE.csv
#   figure/SLOPE_PERC_MAILLEXkm.png
#   figure/SLOPE_PERC_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "geomorph")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load slope data ------------------------------------

# combine tiles
#
# xseq <- c("w010", "w005", "e000", "e005", "e010")
# yseq <- c("n40", "n45", "n50")
# apply(expand.grid(yseq, xseq), 1, paste, collapse = "")
#
# yxseq <- c(
#   "n45w010",
#   "n40w005",
#   "n45w005",
#   "n40e000",
#   "n45e000",
#   "n50e000",
#   "n40e005",
#   "n45e005"
# )
# files <- paste0("slope_90M_", yxseq, ".tif")
# rlist <- lapply(file.path(data_folder, files), rast)
# mos <- mosaic(sprc(rlist))
# writeRaster(mos, file.path(data_folder, "slope_90M_France.tif"))

slope <- rast(file.path(data_folder, "slope_90M_France.tif"))

# 3. Overlay and calculate statistics -----------------------------
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

  exti <- exactextractr::exact_extract(
    slope,
    sf::st_as_sf(shp),
    fun = "mean"
  )

  shp$SLOPE_PERC <- round(exti, 2)

  fi <- paste0("SLOPE_PERC_", labi, ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "SLOPE_PERC",
    border = NA,
    main = paste("Slope (%) -", i),
    breaks = 6,
    breakby = "cases"
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_SLOPE.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
