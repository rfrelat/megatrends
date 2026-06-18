# Script to get socio-economic indicators from Agreste
#
# input:
#   data_Agreste_Commune.csv from https://stats.agriculture.gouv.fr/cartostat/
# output:
#   indicators_csv/COMMUNE_AGRESTE_2020.csv
#   indicators_csv/MAILLE_AGRESTE_2020.csv
#   figure/AGRESTE_XX_COMMUNE.png
#   figure/AGRESTE_XX_MAILLE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "agreste")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
mailles <- terra::vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
# to simplify and fasten the mapping
cross <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))
# add known commune synonyms to improve the number of matches
synonyms <- read.csv(file.path(ref_folder, "commune_synonyms.csv"))

#format the list of reference
ref <- data.frame(
  "name" = clean_city_names(commune$NOM_M),
  "code" = check_postalcode(commune$INSEE_COM),
  "id" = commune$INSEE_COM
)

# data from https://agreste.agriculture.gouv.fr/agreste-web/disaron/RA2020_2006/detail/
#   are not spatially informed so unusable
# fds <- read.csv2(file.path(data_folder, "FDS_RA2020_2006_2020.csv"))
# head(fds)

# data from https://agreste.agriculture.gouv.fr/agreste-web/disaron/RA2020_2052/detail/
#   are not spatially informed so unusable
# fds <- read.csv2(file.path(data_folder, "FDS_RA2020_2052_2020.csv"))
# head(fds)

# 2. Load and pre-process NID data ------------------------------------

## 2A. Age et sexe des exploitants agricoles
ag <- read.table(
  file.path(data_folder, "data_Agreste_Commune.csv"),
  skip = 2,
  sep = ";",
  quote = "\"",
  header = TRUE,
  na.strings = "N/A",
  encoding = "utf8"
)

keepC <- c(
  "Part.des.chefs.d.exploitation.et.coexploitants...40.ans..2020",
  "PBS.moyenne.en.2020",
  "Nombre.d.exploitations.en.2020",
  "SAU.en.2020"
)

labC <- c(
  "AGRESTE_Below40_PCT_2020",
  "AGRESTE_PBS_kEUR_2020",
  "AGRESTE_Nfarms_2020",
  "AGRESTE_SAU_HA_2020"
)

# 3. Merge with commune -----------------------------
m0 <- match(commune$INSEE_COM, ag$Code)
# table(is.na(m0)) # only 11 missing
# table(duplicated(m0[!is.na(m0)])) # no duplicates

commune[, labC] <- ag[m0, keepC]

commune$AGRESTE_PRODUCTIVITY_kEUR_per_ha_2020 <- ifelse(
  commune$AGRESTE_SAU_HA_2020 > 0,
  commune$AGRESTE_PBS_kEUR_2020 / commune$AGRESTE_SAU_HA_2020,
  NA
)

commune$AGRESTE_FARMDENSITY_per_ha_2020 <- ifelse(
  commune$AREA_HA > 0,
  commune$AGRESTE_Nfarms_2020 / commune$AREA_HA,
  NA
)

var <- c(
  labC,
  "AGRESTE_PRODUCTIVITY_kEUR_per_ha_2020",
  "AGRESTE_FARMDENSITY_per_ha_2020"
)

for (i in var) {
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
  file.path(ind_folder, "COMMUNE_AGRESTE_2020.csv"),
  row.names = FALSE
)

## per maille 10km
# load the data
# com_csv <- read.csv(file.path(ind_folder, "COMMUNE_NB_HVE_2024.csv"))
# commune$NB_HVE_2024 <- com_csv$NB_HVE_2024
# make sure the cross match
# table(row.names(cross) == mailles$cd_sig, useNA = "ifany")
# table(colnames(cross) == commune$INSEE_COM, useNA = "ifany")

# weighted average
for (i in var) {
  cross_nb <- t(cross) * data.frame(commune)[, i]
  sum_nb <- apply(cross_nb, 2, sum, na.rm = TRUE)
  nb <- apply(!is.na(cross_nb), 2, sum, na.rm = TRUE)
  mailles[, i] <- ifelse(nb == 0, NA, sum_nb / mailles$AREA_HA)
}

for (i in var) {
  filei <- paste0(toupper(i), "_MAILLE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    mailles,
    y = i,
    border = NA,
    main = paste(i, "- Maille"),
    breaks = 6,
    breakby = "cases"
  )
  dev.off()
}


write.csv(
  data.frame(mailles),
  file.path(ind_folder, "MAILLE_AGRESTE_2020.csv"),
  row.names = FALSE
)
