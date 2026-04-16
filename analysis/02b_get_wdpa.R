# Script to get indicators from the World Database on Protected Areas (WDPA)
#
# input:
#   WDPA_Apr2026_Public_shp-polygons.shp
#     from https://www.protectedplanet.net
# output:
#   indicators_csv/MAILLE_WDPA_2026.csv
#   indicators_csv/COMMUNE_WDPA_2026.csv
#   figure/PROT_AREA_2026_MAILLE.png
#   figure/PROT_AREA_2026_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra) #for fast processing of large files

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "wdpa")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- vect(file.path(ref_folder, "commune_4326.gpkg"))
mailles <- vect(file.path(ref_folder, "mailles_10km_4326.gpkg"))

# 2. Load and clean WDPA data ------------------------------------
if (!file.exists(file.path(out_folder, "WDPA_2026_simp.gpkg"))) {
  shpfiles <- list.files(
    data_folder,
    "^WDPA_Apr2026_Public.*shp$",
    full.names = TRUE
  )
  wdpa_list <- lapply(shpfiles, function(x) {
    vect(x, ext = ext(commune))
  })
  wdpa <- do.call(rbind, wdpa_list)

  # select areas in france
  wdpa <- wdpa[grepl("FRA", wdpa$ISO3), ]

  # fully implemented and in terrestrial realm
  keep <- wdpa$STATUS == "Designated" & wdpa$REALM == "Terrestrial"
  # all VERIF = State Verified
  wdpa <- wdpa[keep, ]

  # table(wdpa$IUCN_CAT, useNA = "ifany")
  # plot(wdpa, y = "IUCN_CAT")
  # View(data.frame(wdpa))
  # rm_cat <- c("Not Reported")
  # Not Reported include RAMSAR sites, habitat and bird directives sites
  # wdpa <- wdpa[!wdpa$IUCN_CAT %in% rm_cat, ]

  # aggregate all polygons
  wdpa_agg <- aggregate(wdpa)

  # simplify polygons
  wdpa_simp <- simplifyGeom(wdpa_agg, tolerance = 0.0001)

  # save the intermediate output
  writeVector(
    wdpa_agg,
    file.path(out_folder, "WDPA_2026_agg.gpkg")
  )
  writeVector(
    wdpa_simp,
    file.path(out_folder, "WDPA_2026_simp.gpkg")
  )
} else {
  wdpa_simp <- vect(file.path(out_folder, "WDPA_2026_simp.gpkg"))
}

# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

## 3a. for mailles 10km
intM <- intersect(wdpa_simp, mailles)
# plot(intM)
# because wdpa was aggregated, there is no duplicated cd_sig
intM$pa <- expanse(intM) * 0.0001
mailles$PROT_AREA_HA <- ifelse(
  mailles$cd_sig %in% intM$cd_sig,
  intM$pa[match(mailles$cd_sig, intM$cd_sig)],
  0
)

#fmt: skip
mailles$PROT_AREA_PCT <- (mailles$PROT_AREA_HA / mailles$AREA_HA * 100) |> 
  round(2)

png(
  file = file.path(fig_folder, "PROT_AREA_2026_MAILLE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  mailles,
  y = "PROT_AREA_PCT",
  # col = map.pal("blues", 100),
  border = NA,
  main = "Protected area (%) - 2026 - Commune"
)
dev.off()

write.csv(
  data.frame(mailles),
  file.path(ind_folder, "MAILLE_WDPA_2026.csv"),
  row.names = FALSE
)

## 3b. for communes
# calculate the intersections
intC <- intersect(wdpa_simp, commune)
# because wdpa was aggregated, there is no duplicated cd_sig

# calculate the area in ha
intC$pa <- expanse(intC) * 0.0001

table(commune$INSEE_COM %in% intC$INSEE_COM)
dim(intC)
head(intM$INSEE_COM)
# add the protected area in the original shapefile
commune$PROT_AREA_HA <- ifelse(
  commune$INSEE_COM %in% intC$INSEE_COM,
  intC$pa[match(commune$INSEE_COM, intC$INSEE_COM)],
  0
)

# calculate the percentage of
# fmt:skip
commune$PROT_AREA_PCT <- (commune$PROT_AREA_HA / commune$AREA_HA * 100) |> 
  round(2)

png(
  file = file.path(fig_folder, "PROT_AREA_2026_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "PROT_AREA_PCT",
  # col = map.pal("blues", 100),
  border = NA,
  main = "Protected area (%) - 2026 - Commune"
)
dev.off()

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_WDPA_2026.csv"),
  row.names = FALSE
)
