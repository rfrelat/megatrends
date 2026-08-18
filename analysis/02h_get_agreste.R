# Script to get socio-economic indicators from Agreste
#
# input:
#   data_Agreste_Commune20X0.csv from https://stats.agriculture.gouv.fr/cartostat/
# output:
#   indicators_csv/COMMUNE_AGRESTE_2020.csv
#   indicators_csv/MAILLEXkm_AGRESTE_2020.csv
#   figure/AGRESTE_XX_COMMUNE.png
#   figure/AGRESTE_XX_MAILLEXkm.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "agreste")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
scales <- c(10, 5, 1) # in km, resolution of the mailles
# mailles <- terra::vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
# cross <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))
# add known commune synonyms to improve the number of matches
# synonyms <- read.csv(file.path(ref_folder, "commune_synonyms.csv"))

#format the list of reference
# ref <- data.frame(
#   "name" = clean_city_names(commune$NOM_M),
#   "code" = check_postalcode(commune$INSEE_COM),
#   "id" = commune$INSEE_COM
# )

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
  file.path(data_folder, "data_Agreste_Commune2020.csv"),
  skip = 2,
  sep = ";",
  quote = "\"",
  header = TRUE,
  na.strings = "N/A",
  encoding = "utf8"
)
# table(commune$INSEE_COM %in% ag$Code) # only 11 missing
keepC <- c(
  "Part.des.chefs.d.exploitation.et.coexploitants...40.ans..2020",
  "PBS.moyenne.en.2020",
  "Nombre.d.exploitations.en.2020",
  "SAU.en.2020",
  "Part.de.la.superficie.irriguée.dans.la.SAU..2020"
)

labC <- c(
  "AGRESTE_Below40_PCT_2020",
  "AGRESTE_PBS_kEUR_2020",
  "AGRESTE_Nfarms_2020",
  "AGRESTE_SAU_HA_2020",
  "AGRESTE_IRRIGATED_PERC_2020"
)


# ag10 <- read.table(
#   file.path(data_folder, "data_Agreste_Commune2010.csv"),
#   skip = 2,
#   sep = ";",
#   quote = "\"",
#   header = TRUE,
#   na.strings = "N/A",
#   encoding = "utf8"
# )
ag10 <- read.csv(
  file.path(data_folder, "data_Agreste_Commune2010.csv")
)
keepC10 <- c(
  "Chefs.d.exploitation.et.coexploitants.avec.formation.supérieure...part.en.2010",
  "SAU.moyenne.par.UTA.2010"
)

labC10 <- c(
  "AGRESTE_EDUSUP_PCT_2010",
  "AGRESTE_FARMSIZE_HA_2010"
)
# table(commune$INSEE_COM %in% ag10$Code) # only 7 missing

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

m1 <- match(commune$INSEE_COM, ag10$Code)
# table(is.na(m1)) # only 7 missing
# table(duplicated(m1[!is.na(m1)])) # no duplicates

# make sure it is numeric
suppressWarnings({
  ag10[, keepC10] <- apply(ag10[, keepC10], 2, as.numeric)
})

commune[, labC10] <- ag10[m1, keepC10]

var <- c(
  labC,
  "AGRESTE_PRODUCTIVITY_kEUR_per_ha_2020",
  "AGRESTE_FARMDENSITY_per_ha_2020",
  labC10
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
  for (j in var) {
    cross[, j] <- data.frame(commune)[m0, j]

    sumJ <- tapply(cross[, j] * cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)
    numJ <- tapply(!is.na(cross[, j]), cross$cd_sig, sum, na.rm = TRUE)

    indj <- ifelse(numJ > 0, sumJ / area, NA)
    mailles[, j] <- as.numeric(indj[m1])

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
    file.path(ind_folder, paste0("MAILLE", i, "km_AGRESTE_2020.csv")),
    row.names = FALSE
  )
}
