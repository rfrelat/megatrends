# Script to get the hedgerow density from BD Haie
#
# input:
#   BD Haie from https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_BD-HAIE
#     bdhaie/haie_2-0.gpkg
#
# output:
#   indicators_csv/MAILLEXkm_BDHAIE_2020.csv
#   indicators_csv/COMMUNE_BDHAIE_2020.csv
#   figure/BDHAIE_DENSITY_2020_MAILLEXkm.png
#   figure/BDHAIE_DENSITY_2020_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "bdhaie")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

# scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")
scales <- c("mailles_10km", "mailles_5km", "mailles_1km")
# 2. Load and clean OSM data ------------------------------------
# https://docs.ropensci.org/osmextract/
bdhaie <- vect(file.path(data_folder, "haie_2-0.gpkg"))

# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale
# especially for commune

# Same process for commune and maille
for (i in scales) {
  print(i)
  if (i == "commune") {
    shp <- vect(file.path(ref_folder, "commune_2154.gpkg"))
    shp$id <- shp$INSEE_COM
  } else {
    shp <- vect(file.path(ref_folder, paste0(i, "_2154.gpkg")))
    shp$id <- shp$cd_sig
  }

  labi <- gsub("KM$", "km", toupper(gsub("_", "", i)))

  # hedgerows
  inti_he <- intersect(bdhaie, shp)
  inti_he$length <- perim(inti_he)
  he_sumid <- tapply(inti_he$length, inti_he$id, sum, na.rm = TRUE)
  m_he <- match(shp$id, names(he_sumid))

  shp$BDHAIE_HEDGE_m_2020 <- ifelse(
    is.na(m_he),
    0,
    round(he_sumid[m_he] / 1000, 3)
  )

  #fmt:skip
  shp$BDHAIE_HEDGEDENSITY_m_per_ha_2020 <- shp$BDHAIE_HEDGE_m_2020 * 1000 / shp$AREA_HA

  var <- c(
    "BDHAIE_HEDGE_m_2020",
    "BDHAIE_HEDGEDENSITY_m_per_ha_2020"
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
      breaks = 6,
      breakby = "cases",
      main = paste(j, "-", i)
    )
    dev.off()
  }

  # export
  out_fi <- paste0(gsub("S", "", labi), "_BDHAIE_2020.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
