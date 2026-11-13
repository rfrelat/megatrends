# Script to get indicators of N and P concentration from European Env. Agency
#
# input:
#   eea/N_P.shp
#     https://www.eea.europa.eu/en/datahub/datahubitem-view/edbbd466-b845-4e4c-acf9-905ec5e28766?activeAccordion=1083479
#     eea_v_3035_1_km_n-p-agri-soil_p_2008-2019_v01_r00
#
# output:
#   indicators_csv/MAILLEXkm_NP_2018.csv
#   indicators_csv/COMMUNE_NP_2018.csv
#   figure/XX_2018_MAILLEXkm.png
#   figure/XX_2018_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "eea")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load and clean WDPA data ------------------------------------
np <- vect(file.path(data_folder, "N_P.shp"))
# project to get exact match of CRS
np <- project(np, "EPSG:3035")

# which information is relevant?
# "Nsurp", "Psurp": Nitrogen/Phosphorus Surplus
# "NUE",  "PUE": Nitrogen/Phosphorus Use Efficiency
# "DNUEact", "DNUEtar", "DPUE" : detla N/P Use efficiency (actual or target)
# "NH3em", "N2Oem": emmission of NH3 or N2O
# "Nlegw", "Nlesw", "Ple": Nitrogen/Phosphorous leaching to ground/soil water
# "Pacc": Phosphorus accumulation
# "PSD1975", "PSD2010": Phosphorus Saturation Degree in 1975 and 2010

# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

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

  int <- intersect(np, shp)
  # dim(int)
  int$calc_area <- expanse(int) * 0.0001
  # compute weighted mean
  sum_area <- tapply(int$calc_area, int$id, sum, na.rm = TRUE)
  Nsu <- tapply(int$Nsurp * int$calc_area, int$id, sum, na.rm = TRUE) / sum_area
  NUE <- tapply(int$NUE * int$calc_area, int$id, sum, na.rm = TRUE) / sum_area
  Psu <- tapply(int$Psurp * int$calc_area, int$id, sum, na.rm = TRUE) / sum_area

  # table(shp$id %in% names(sum_area))
  m0 <- match(shp$id, names(sum_area))

  shp$Nsurplus_KG_per_HA_2018 <- Nsu[m0]
  shp$NUE_perc_2018 <- NUE[m0]
  shp$Psurplus_KG_per_HA_2018 <- Psu[m0]

  var <- c(
    "Nsurplus_KG_per_HA_2018",
    "NUE_perc_2018",
    "Psurplus_KG_per_HA_2018"
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
  out_fi <- paste0(gsub("S", "", labi), "_NP_2018.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
