# Script to get socio-economic indicators from Agreste
#
# input:
#   data_Agreste_Commune.csv from https://stats.agriculture.gouv.fr/cartostat/
# output:
#   indicators_csv/COMMUNE_NITRATE_2025.csv
#   indicators_csv/MAILLE_NITRATE_2025.csv
#   figure/NITRATE_2025_XX_COMMUNE.png
#   figure/NITRATE_2025_XX_MAILLE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "agreste")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
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
  "Perc_below40_2020",
  "PBS_kEUR_2020",
  "N_farms_2020",
  "SAU_ha_2020"
)

# clean the name and code
ag$name <- clean_city_names(ag$Libellé)
ag$code <- check_postalcode(ag$Code)

# change INSEE code to code (instead of postal code, not here)e
synonyms$code <- synonyms$id


# 3. Merge with commune -----------------------------

m0 <- simple_match_cities(ag, ref)
table(is.na(m0)) # only 352 missing (1%)

# super long, not sure why
# m1 <- match_cities(
#   ag,
#   ref,
#   dfsyn = synonyms,
#   dmax = 0.25,
#   file.out = file.path(out_folder, "fuzzy_Agreste.csv")
# )
# Number of simple match: 34616 (99%)
# Number of verified synonyms: 0 (0%)
# Number of fuzzy match: 0 (0%)
# Number of non-matching elements: 352(1%)

## export --------------------------------------------------
## per commune
m0 <- match(commune$INSEE_COM, ag$Code)

commune[, labC] <- ag[m0, keepC]

for (i in labC) {
  filei <- paste0("AGRESTE_", toupper(i), "_COMMUNE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(commune, y = i, border = NA, main = paste("Agreste -", i, "- Commune"))
  dev.off()
}

write.csv(
  data.frame(commune),
  file.path(out_folder, "COMMUNE_AGRESTE.csv"),
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
for (i in labC) {
  cross_nb <- t(cross) * data.frame(commune)[, i]
  sum_nb <- apply(cross_nb, 2, sum, na.rm = TRUE)
  mailles[, i] <- sum_nb / mailles$AREA_HA
}

for (i in labC) {
  filei <- paste0("AGRESTE_", toupper(i), "_MAILLE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(mailles, y = i, border = NA, main = paste("Agreste -", i, "- Maille"))
  dev.off()
}


write.csv(
  data.frame(mailles),
  file.path(out_folder, "MAILLE_AGRESTE.csv"),
  row.names = FALSE
)
