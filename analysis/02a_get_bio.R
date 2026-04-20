# Script to get indicators from the World Database on Protected Areas (WDPA)
#
# input:
#   bio from Cartobio: https://www.data.gouv.fr/datasets/parcelles-certifiees-en-agriculture-biologique-sur-cartobio
#   more complete than organic declared at CAP:
#   https://www.data.gouv.fr/datasets/parcelles-en-agriculture-biologique-ab-declarees-a-la-pac
#
# output:
#   indicators_csv/MAILLE_Bio_2024.csv
#   indicators_csv/COMMUNE_Bio_2024.csv
#   figure/BIO_AREA_2024_MAILLE.png
#   figure/BIO_AREA_2024_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "bio")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- vect(file.path(ref_folder, "commune_2154.gpkg"))
mailles <- vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))

# 2. Load and clean WDPA data ------------------------------------
if (!file.exists(file.path(out_folder, "Cartobio_2024_valid.gpkg"))) {
  # check cartobio but not really different
  cartobio <- vect(file.path(
    data_folder,
    "cartobio_parcelles_2024_francemet_2154.gpkg"
  ))
  # table(cb$convers) : certified and in conversion
  #AB     AB?      C1      C2      C3
  # sum(cartobio$surface) # 2863472

  # bio22 <- vect(file.path(
  #   data_folder,
  #   "rpg-bio-2022-national.shp"
  # ))
  # sum(bio$surface_ha) # 2653906

  # make sure the geometry is valid
  bio_valid <- makeValid(cartobio)

  # calculate union (only if polygons overlap): too long, not needed
  # bio_union <- union(bio_valid)
  # aggregate all polygons (too long and not needed)
  # bio_agg <- aggregate(bio_valid)

  # save the intermediate output
  writeVector(
    bio_valid,
    file.path(out_folder, "Cartobio_2024_valid.gpkg")
  )
} else {
  bio_valid <- vect(file.path(out_folder, "Cartobio_2024_valid.gpkg"))
}

# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

## 3a. for mailles 10km
intM <- intersect(bio_valid, mailles)
# is there overlapping polygons? should we do union first?
intM$calc_area <- expanse(intM) * 0.0001
sum_areaM <- tapply(intM$calc_area, intM$cd_sig, sum, na.rm = TRUE)

mailles$BIO_AREA_HA <- ifelse(
  mailles$cd_sig %in% names(sum_areaM),
  sum_areaM[match(mailles$cd_sig, names(sum_areaM))],
  0
)

# check if errors (overlapping areas?)
# table(mailles$BIO_AREA_HA > mailles$AREA_HA)
# looks good :)

#fmt: skip
mailles$BIO_AREA_PCT <- (mailles$BIO_AREA_HA / mailles$AREA_HA * 100) |> 
  round(2)

# calculate the number of fields
n_areaM <- tapply(intM$calc_area > 0, intM$cd_sig, sum, na.rm = TRUE)
mailles$BIO_FIELDS_N <- ifelse(
  mailles$cd_sig %in% names(n_areaM),
  n_areaM[match(mailles$cd_sig, names(n_areaM))],
  0
)
# barplot(table(mailles$BIO_FIELDS_N))

png(
  file = file.path(fig_folder, "BIO_AREA_2024_MAILLE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  mailles,
  y = "BIO_AREA_PCT",
  # col = map.pal("blues", 100),
  border = NA,
  main = "Organic fields (%) - 2024 - Commune"
)
dev.off()


write.csv(
  data.frame(mailles),
  file.path(ind_folder, "MAILLE_BIO_2024.csv"),
  row.names = FALSE
)

## 3b. for communes
# calculate the intersections
intC <- intersect(bio_valid, commune)

# is there overlapping polygons? should we do union first?
intC$calc_area <- expanse(intC) * 0.0001
# sum of the organic cultivated area per commune
sum_areaC <- tapply(intC$calc_area, intC$INSEE_COM, sum, na.rm = TRUE)

# add the organic cultivated area in the original shapefile
commune$BIO_AREA_HA <- ifelse(
  commune$INSEE_COM %in% names(sum_areaC),
  sum_areaC[match(commune$INSEE_COM, names(sum_areaC))],
  0
)

# check if errors (overlapping areas?)
table(commune$BIO_AREA_HA > commune$AREA_HA)
# looks good :)

# calculate the percentage of
# fmt:skip
commune$BIO_AREA_PCT <- (commune$BIO_AREA_HA / commune$AREA_HA * 100) |> 
  round(2)


# calculate the number of fields
n_areaC <- tapply(intC$calc_area > 0, intC$INSEE_COM, sum, na.rm = TRUE)
commune$BIO_FIELDS_N <- ifelse(
  commune$INSEE_COM %in% names(n_areaC),
  n_areaC[match(commune$INSEE_COM, names(n_areaC))],
  0
)
# barplot(table(commune$BIO_FIELDS_N))

png(
  file = file.path(fig_folder, "BIO_AREA_2024_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "BIO_AREA_PCT",
  # col = map.pal("blues", 100),
  border = NA,
  main = "Organic fields (%) - 2024 - Commune"
)
dev.off()

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_BIO_2024.csv"),
  row.names = FALSE
)
