# Script to get the Scherpenhuijzen 2025 forest data
#   Scherpenhuijzen, N., West, T.A.P., Debonne, N., ... , Verburg, P.H., 2025.
#     Mapping forest management regimes in Europe. Forest Ecology and Management
#     https://doi.org/10.1016/j.foreco.2025.122940
#   https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/HQIJN5
#
# input:
#   scherpenhuijzen2025/Forest_management_map_raster.tif # 1km resolution
#   scherpenhuijzen2025/meff_2018_EEA38plusUK_1km_32bit_float_final.tif #1km resolution
# output:
#   indicators_csv/MAILLEXkm_FOREST_2023.csv
#   indicators_csv/COMMUNE_FOREST_2023.csv
#   figure/MOWING_EVENTS_MAILLEXkm.png
#   figure/MOWING_EVENTS_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "scherpenhuijzen2025")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load fragmentation data ------------------------------------
list.files(data_folder)
# forest management class
forest <- rast(file.path(data_folder, "Forest_management_map_raster.tif"))
convForest <- c(
  "1" = "1.Unmanaged forest",
  "2" = "2.Close-to-nature forestry",
  "3" = "3.Combined objective forestry",
  "4" = "4.Intensive forestry",
  "5" = "5.Very intensive forestry"
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
  shp <- project(shp, forest)
  labi <- gsub("KM$", "km", toupper(gsub("_", "", i)))
  # extract the majority per polygon
  fori <- exactextractr::exact_extract(
    forest,
    sf::st_as_sf(shp),
    fun = "mode"
  )
  # tranform with labels
  shp$FOREST_MGT_2023 <- convForest[fori]

  png(
    file = file.path(fig_folder, paste0("FOREST_MGT_2023", labi, ".png")),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "FOREST_MGT_2023",
    border = NA,
    main = paste("FOREST_MGT_2023 -", i),
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_FOREST_2023.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
