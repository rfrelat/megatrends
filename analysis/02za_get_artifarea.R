# Script to get percentage of artificialisation area
#   https://www.data.gouv.fr/datasets/consommation-despaces-naturels-agricoles-et-forestiers-du-1er-janvier-2009-au-1er-janvier-2024
#
# input:
#   raw-data/cerema/obs_artif_conso_com_2009_2024_carroyage_LEA.shp
#   raw-data/cerema/obs_artif_conso_com_2009_2024.shp
#
# output:
#   indicators_csv/MAILLEXkm_ARTIF_AREA.csv
#   indicators_csv/COMMUNE_ARTIF_AREA.csv
#   figure/ARTIF_AREA_XX_2009_2024_MAILLEXkm.png
#   figure/ARTIF_AREA_XX_2009_2024_COMMUNE.png
#
#
# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra) #for fast processing of large files

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "cerema")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load and clean WDPA data ------------------------------------

# Artificialization per 1km
artif1km <- vect(
  file.path(data_folder, "obs_artif_conso_com_2009_2024_carroyage_LEA.shp")
)

# Artificialization per commune
artifCom <- vect(
  file.path(data_folder, "obs_artif_conso_com_2009_2024.shp")
)
# names(artif)
# interested in
#   naf09art24: total des flux entre NAF et artificialisé sur la période 2009-2024
#   artcom0924: part de surface communale convertie en surface artificialisée (en %)
# sum(expanse(artif1km) * 0.0001) # 58660000 ha
# sum(expanse(artifCom) * 0.0001) # 54915485 ha

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

  # intersect only if not commune
  if (i == "commune") {
    mc <- match(shp$INSEE_COM, artifCom$idcom)
    # table(is.na(mc)) only 21 missing values
    shp$ARTIF_AREA_HA_2009_2024 <- round(artifCom$naf09art24[mc] * 0.0001, 2)
    shp$ARTIF_AREA_PERC_2009_2024 <- round(artifCom$artcom0924[mc], 3)
  } else {
    int <- intersect(artif1km, shp)

    int$calc_area <- expanse(int)
    # compute weighted mean
    sum_area <- tapply(int$calc_area, int$id, sum, na.rm = TRUE)
    arti <- tapply(
      int$naf09art24 * int$calc_area * 0.0001,
      int$id,
      sum,
      na.rm = TRUE
    ) /
      sum_area

    # table(shp$id %in% names(sum_area))
    m0 <- match(shp$id, names(sum_area))

    shp$ARTIF_AREA_HA_2009_2024 <- round(arti[m0], 2)
    shp$ARTIF_AREA_PERC_2009_2024 <- round(arti[m0] / shp$AREA_HA * 100, 3)
  }

  var <- c("ARTIF_AREA_HA_2009_2024", "ARTIF_AREA_PERC_2009_2024")
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
  out_fi <- paste0(gsub("S", "", labi), "_ARTIF_AREA.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
