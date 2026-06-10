# Script to get indicators from pesticide use
#
# input:
#   pesticide from Rigal and Perrot 2025
#     from https://datadryad.org/dataset/doi:10.5061/dryad.g4f4qrg0p
#     mean_concentration_air: Summed concentration of the 99 toxic, carcinogenic, mutagenic, reprotoxic active substances in the air (ng.m-³)
#     mean_tii: Summed treatment intensity index of of the 175 toxic, carcinogenic, mutagenic, reprotoxic active substances used
#     mean_concentration_water: Summed concentration of the 145 toxic, carcinogenic, mutagenic, reprotoxic active substances in the surface water (ng.m-³)
#     all_pesticide_exposure: Combined exposure in toxic, carcinogenic, mutagenic, reprotoxic active substances from the scaled concentrations in air and water and treatment intensity index, designed to vary between 0 and 3 (historical data between 0 and 1.5)
#
# output:
#   indicators_csv/MAILLE_Pesticide.csv
#   indicators_csv/COMMUNE_Pesticide.csv
#   figure/PESTICIDE_XX_MAILLE.png
#   figure/PESTICIDE_XX_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "pesticide")
out_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- vect(file.path(ref_folder, "commune_4326.gpkg"))
mailles <- vect(file.path(ref_folder, "mailles_10km_4326.gpkg"))

# project to RGF93 v1 / Lambert-93 (EPSG:2154)
commune <- project(commune, "EPSG:2154")
mailles <- project(mailles, "EPSG:2154")

# 2. Load pesticide data ------------------------------------
exp <- vect(file.path(
  data_folder,
  "Combined_exposure_to_active_substance_in_use_air_and_water.gpkg"
))
names(exp)
plot(exp, y = "mean_concentration")

# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

## 3a. for mailles 10km
intM <- intersect(exp, mailles)

# calculate intersecting area
intM$pa <- expanse(intM) * 0.0001

npix <- table(intM$cd_sig)
suma <- tapply(intM$pa, intM$cd_sig, sum, na.rm = TRUE)
# table(names(suma) == names(npix))

#fmt:skip
out <- data.frame(
  "cd_sig" = names(npix),
  "npix" = as.numeric(npix),
  "mean_concentration_air" = tapply(intM$mean_concentration*intM$pa, intM$cd_sig, sum, na.rm = TRUE) / suma,
  "mean_concentration_water" = tapply(intM$mean_concentration_water*intM$pa, intM$cd_sig, sum, na.rm = TRUE) / suma,
  "treatment_intensity_index" = tapply(intM$mean_itt*intM$pa, intM$cd_sig, sum, na.rm = TRUE) / suma,
  "pesticide_exposure" = tapply(intM$all_pesticide_exposure*intM$pa, intM$cd_sig, sum, na.rm = TRUE) / suma
)

m0 <- match(mailles$cd_sig, out$cd_sig)
keepC <- names(out)[!names(out) %in% names(mailles)]
# plot(intM)

mailles[, keepC] <- out[m0, keepC]

for (i in keepC) {
  filei <- paste0("PESTICIDE_", toupper(i), "_MAILLE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(mailles, y = i, border = NA, main = paste("Pesticide -", i, "- Maille"))
  dev.off()
}

write.csv(
  data.frame(mailles),
  file.path(out_folder, "MAILLE_PESTICIDE.csv"),
  row.names = FALSE
)

## 3b. for communes
# calculate the intersections
intC <- intersect(exp, commune)
# because wdpa was aggregated, there is no duplicated cd_sig

# calculate the area in ha
intC$pa <- expanse(intC) * 0.0001

npix <- table(intC$INSEE_COM)
suma <- tapply(intC$pa, intC$INSEE_COM, sum, na.rm = TRUE)

#fmt:skip
out <- data.frame(
  "INSEE_COM" = names(npix),
  "npix" = as.numeric(npix),
  "mean_concentration_air" = tapply(intC$mean_concentration*intC$pa, intC$INSEE_COM, sum, na.rm = TRUE) / suma,
  "mean_concentration_water" = tapply(intC$mean_concentration_water*intC$pa, intC$INSEE_COM, sum, na.rm = TRUE) / suma,
  "treatment_intensity_index" = tapply(intC$mean_itt*intC$pa, intC$INSEE_COM, sum, na.rm = TRUE) / suma,
  "pesticide_exposure" = tapply(intC$all_pesticide_exposure*intC$pa, intC$INSEE_COM, sum, na.rm = TRUE) / suma
)

# match the rows
m0 <- match(commune$INSEE_COM, out$INSEE_COM)
# select the column to keep
keepC <- names(out)[!names(out) %in% names(commune)]

commune[, keepC] <- out[m0, keepC]

for (i in keepC) {
  filei <- paste0("PESTICIDE_", toupper(i), "_COMMUNE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(commune, y = i, border = NA, main = paste("Pesticide -", i, "- Commune"))
  dev.off()
}

write.csv(
  data.frame(commune),
  file.path(out_folder, "COMMUNE_PESTICIDE.csv"),
  row.names = FALSE
)
