# Script to get the hedgerow density from Liu et al. 2023
#
# input:
#   https://zenodo.org/records/8154445
#   raw-data/liu2023/planet_agb_30m_v0.1.tif
#   raw-data/liu2023/planet_canopy_cover_30m_v0.1.tif
#   raw-data/liu2023/planet_canopy_height_30m_v0.1.tif
#
# output:
#   indicators_csv/MAILLEXkm_TREE_2019.csv
#   indicators_csv/COMMUNE_TREE_2019.csv
#   figure/TREE_XX_2019_MAILLEXkm.png
#   figure/TREE_XX_2019_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "liu2023")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load fragmentation data ------------------------------------
# slower if files are loaded one by one
# biomass <- rast(file.path(data_folder, "planet_agb_30m_v0.1.tif"))
# cover <- rast(file.path(data_folder, "planet_canopy_cover_30m_v0.1.tif"))
# height <- rast(file.path(data_folder, "planet_canopy_height_30m_v0.1.tif"))
# could be faster if all liu 2023 rasters are in a single object
# but need maximum height
liu_files <- c(
  "planet_agb_30m_v0.1.tif",
  "planet_canopy_cover_30m_v0.1.tif",
  "planet_canopy_height_30m_v0.1.tif"
)
liu <- terra::rast(file.path(data_folder, liu_files))
names(liu) <- c("biomass", "cover", "height")

lab <- c(
  "TREE_BIOMASS_KG_2019",
  "TREE_COVER_PERC_2019",
  "TREE_HEIGHT_M_2019"
)

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

  liui <- exactextractr::exact_extract(
    liu,
    sf::st_as_sf(shp),
    fun = "mean"
  )

  names(liui) <- names(liu)
  liui[is.na(liui)] <- 0
  shp[, lab] <- round(liui, 2)

  for (j in lab) {
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
  out_fi <- paste0(gsub("S", "", labi), "_TREE_2019.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
