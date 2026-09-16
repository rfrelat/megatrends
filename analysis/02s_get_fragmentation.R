# Script to get the landscape fragmentation
#  Two dataset exists at European scale for 2018
#  -Effective Mesh Size estimates the average size of unfragmented land areas ("meshes") that remain free from barriers
#   such as roads, railways, urban areas, and other man-made infrastructure.
#   Larger mesh sizes indicate more connected landscapes, while smaller mesh sizes reflect higher levels of fragmentation.
#   https://www.eea.europa.eu/en/datahub/datahubitem-view/d4a5a6a0-0d0b-4a0d-9cef-211e452cf2e1
#  -Effective Mesh Density is a measure of the degree to which movement between different parts of the landscape is interrupted by a Fragmentation Geometry (FG).
#   FGs are defined as the presence of impervious surfaces and traffic infrastructure, including medium sized roads.
#   The more FGs fragment the landscape, the higher the effective mesh density hence the higher the fragmentation.
#   https://www.eea.europa.eu/en/datahub/datahubitem-view/9d0b51f9-047d-4af1-89eb-3756e46ffc53
#
# input:
#   fragmentation/SEFF_2018_10m.tif # 100m resolution
#   fragmentation/meff_2018_EEA38plusUK_1km_32bit_float_final.tif #1km resolution
# output:
#   indicators_csv/MAILLEXkm_FRAGMENTATION.csv
#   indicators_csv/COMMUNE_FRAGMENTATION.csv
#   figure/FRAG_XX_MAILLEXkm.png
#   figure/FRAG_XX_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "fragmentation")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load fragmentation data ------------------------------------
meff <- rast(file.path(
  data_folder,
  "meff_2018_EEA38plusUK_1km_32bit_float_final.tif"
))
seff <- rast(file.path(data_folder, "SEFF_2018_10m.tif"))
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

  meffi <- exactextractr::exact_extract(
    meff,
    sf::st_as_sf(shp),
    fun = "mean"
  )

  shp$FRAG_MEFF_2018 <- round(meffi, 3)

  seffi <- exactextractr::exact_extract(
    seff,
    sf::st_as_sf(shp),
    fun = "mean"
  )

  shp$FRAG_SEFF_2018 <- round(seffi, 1)

  var <- c(
    "FRAG_MEFF_2018",
    "FRAG_SEFF_2018"
  )
  for (j in var) {
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
  out_fi <- paste0(gsub("S", "", labi), "_FRAGMENTATION.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
