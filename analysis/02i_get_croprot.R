# Script to get crop rotation indicators from
#
# input:
#   croprot/indicateurs_successions_2015-2023.csv
#     from https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/WQ4MIF
#     - Nombre de cultures: N_CULT
#     - Indice de Simpson inversé: INV_SIM
#     - Nombre d'occurrence de la culture dominante de la rotation: N_MAX	(Nombre maximal d'occurence d'une même culture)
#     - Nombre de prairies temporaires présentes dans les séquences: N_PT
#     - Nombre de prairies temporaires présentes consécutivement dans les séquences, moyenne: N_PT_CONS
# output:
#   indicators_csv/COMMUNE_CROPROT_1523.csv
#   indicators_csv/MAILLEXkm_CROPROT_1523.csv
#   figure/CROPROT_XX_COMMUNE.png
#   figure/CROPROT_XX_MAILLEXkm.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "croprot")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
scales <- c(10, 5, 1) # in km, resolution of the mailles
# mailles <- terra::vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
# cross <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))

# 2. Load and pre-process NID data ------------------------------------

## 2A. Age et sexe des exploitants agricoles
rot <- read.csv(
  file.path(data_folder, "indicateurs_successions_2015-2023.csv")
)
rot <- rot[rot$AB15_23 == "Ensemble", ]
rot <- rot[!rot$insee_com %in% c("REG", "DEP"), ]
# table(duplicated(rot$insee_com)) no duplicates anymore

keepC <- c(
  "N_CULT",
  "INV_SIM",
  "N_MAX",
  "N_PT",
  "N_PT_CONS"
)

labC <- c(
  "CROPROT_N_CULT_1523",
  "CROPROT_INV_SIM_1523",
  "CROPROT_N_MAX_1523",
  "CROPROT_N_PT_1523",
  "CROPROT_N_PT_CONS_1523"
)

# 3. Merge with commune -----------------------------
m0 <- match(commune$INSEE_COM, rot$insee_com)
# table(is.na(m0)) # 2793 missing
# table(duplicated(m0[!is.na(m0)])) # no duplicates
# table(rot$insee_com %in% commune$INSEE_COM) # 70 missing INSEE_com
# so possibly 70 communes could be saved

commune[, labC] <- rot[m0, keepC]

for (i in labC) {
  filei <- paste0(toupper(i), "_COMMUNE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    commune,
    y = i,
    border = NA,
    main = paste(i, "- Commune"),
    breaks = 6,
    breakby = "cases"
  )
  dev.off()
}

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_CROPROT_1523.csv"),
  row.names = FALSE
)

## per maille
for (i in scales) {
  cat(paste("Maille", i, "km \n"))
  # load the data
  mailles <- terra::vect(
    file.path(ref_folder, paste0("mailles_", i, "km_2154.gpkg"))
  )
  cross <- readRDS(
    file.path(ref_folder, paste0("cross_mailles", i, "km_commune.rds"))
  )

  m0 <- match(cross$INSEE_COM, commune$INSEE_COM)
  area <- tapply(cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)
  m1 <- match(mailles$cd_sig, names(area))
  cross <- as.data.frame(cross)

  # weighted average
  for (j in labC) {
    cross[, j] <- data.frame(commune)[m0, j]

    sumJ <- tapply(cross[, j] * cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)
    numJ <- tapply(!is.na(cross[, j]), cross$cd_sig, sum, na.rm = TRUE)

    indj <- ifelse(numJ > 0, sumJ / area, NA)
    mailles[, j] <- indj[m1]

    filej <- paste0(toupper(j), "_MAILLE", i, "km.png")
    png(
      file = file.path(fig_folder, filej),
      width = 1200,
      height = 1000,
      res = 200
    )
    plot(
      mailles,
      y = j,
      border = NA,
      main = paste(j, "- Maille", i, "km"),
      breaks = 6,
      breakby = "cases"
    )
    dev.off()
  }

  write.csv(
    data.frame(mailles),
    file.path(ind_folder, paste0("MAILLE", i, "km_CROPROT_1523.csv")),
    row.names = FALSE
  )
}
