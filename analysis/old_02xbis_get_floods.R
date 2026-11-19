# Script to get indicators from the Georisques database
#   Territoires à Risque Important d'Inondation (TRI)
# issue: Focuses strictly on major urban areas and high-exposure coastlines.
#   but excludes rural areas and small towns. (only 2,560 communes)
#   https://pinea.app.carto.com/map/
# issue2: all other national data are per department
#   PPRI (Plans de Prévention des Risques d’Inondation)
#   AZI (Atlas des Zones Inondables)
#   EAIP (Enveloppes Approchées d’Inondations Potentielles)
#   not homogeneous for the whole france and patchy (some department/region missing) :(
#   apart maybe https://www.ign.fr/atlas-cartographier-anthropocene-2025-risque-inondation
#   but can't get access to the spatial dataset.
# input:
#   tri_2020/
#     from https://www.georisques.gouv.fr/donnees/bases-de-donnees/zonages-inondation-rapportage-2020
#
# output:
#   indicators_csv/MAILLEXkm_WDPA_2026.csv
#   indicators_csv/COMMUNE_WDPA_2026.csv
#   figure/PROT_AREA_PCT_2026_MAILLEXkm.png
#   figure/PROT_AREA_PCT_2026_COMMUNE.png
#
# WARNING: super long to compute statistics for 1km maille
#
# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra) #for fast processing of large files

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "tri_2020")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load and clean WDPA data ------------------------------------

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/CEMS-EFAS/flood_hazard/Europe_RP100_filled_depth.tif"
# Flood extent map
fext <- vect("data/raw-data/tri_2020/n_carte_inond_s.shp")
# 3668 shapes
# table(fext$typ_inond1)
#   01 (River flooding): 2511
#   02 (Marine flooding): 75
#   03 (Combination): 1051
# table(fext$scenario)
#   01For (frequent scenario): 983,
#   02Moy (medium scenario): 999,
#   03Mcc (medium + climate change): 232
#   04Fai (low probability): 996
# should we select only some of the extent?
sum(expanse(fext) * 0.0001) # 27690188 ha
fext_agg <- aggregate(fext)
sum(expanse(fext_agg) * 0.0001) # 5049700 ha !

# Flood risk map
frisk <- vect("data/raw-data/tri_2020/n_carte_risq_s.shp")
# remove the id column (to avoid duplicated name with shp)
frisk <- frisk[, names(frisk) != "id"]
# 1163 shapes
# but many are overlaid
# sum(expanse(frisk) * 0.0001) # 8776466 ha !

frisk_agg <- aggregate(frisk)
# sum(expanse(frisk_agg) * 0.0001) # 5661990 ha !

# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

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

  int <- intersect(frisk_agg, shp)

  int$calc_area <- expanse(int) * 0.0001

  # compute weighted mean
  risk_area <- tapply(int$calc_area, int$id, sum, na.rm = TRUE)

  m0 <- match(shp$id, names(risk_area))

  shp$FLOOD_RISK_AREA_HA_2020 <- ifelse(is.na(m0), 0, round(risk_area[m0], 3))

  shp$FLOOD_RISK_AREA_PCT_2020 <-
    round(shp$FLOOD_RISK_AREA_HA_2020 / shp$AREA_HA * 100, 2)

  # mostly issue when rounding values for 1km cells
  shp$FLOOD_RISK_AREA_PCT_2020[shp$FLOOD_RISK_AREA_PCT_2020 > 100] <- 100

  fj <- paste0("FLOOD_RISK_AREA_PCT_2020_", labi, ".png")
  png(
    file = file.path(fig_folder, fj),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "FLOOD_RISK_AREA_PCT_2020",
    border = NA,
    main = paste("FLOOD_RISK_AREA_PCT_2020 -", i),
    breaks = 6
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_FLOODS_2020.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
