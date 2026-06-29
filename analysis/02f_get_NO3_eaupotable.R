# Script to get nitrate levels from EauPotable database

# input:
#   eaupotable/dis-2025 from
#     https://www.data.gouv.fr/datasets/resultats-du-controle-sanitaire-de-leau-distribuee-commune-par-commune
# output:
#   indicators_csv/COMMUNE_NITRATE_2025.csv
#   indicators_csv/MAILLEXkm_NITRATE_2025.csv
#   figure/NITRATE_2025_XX_COMMUNE.png
#   figure/NITRATE_2025_XX_MAILLEXkm.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "eaupotable", "dis-2025")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
scales <- c(10, 5, 1) # in km, resolution of the mailles
# mailles <- terra::vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
# cross <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))

# add known commune synonyms to improve the number of matches
# synonyms <- read.csv(file.path(ref_folder, "commune_synonyms.csv"))

# 2. Load and clean HVE data ------------------------------------

## 2A. file with overall result per communes
displv <- read.table(
  file.path(data_folder, "DIS_PLV_2025.txt"),
  sep = ",",
  header = TRUE
)
# table(displv$inseecommuneprinc %in% commune$INSEE_COM) # 8788 missing (mostly 97XXX: DOM-TOM)
# missid <- !displv$inseecommuneprinc %in% commune$INSEE_COM
# displv$inseecommuneprinc[missid]
# table(commune$INSEE_COM %in% displv$inseecommuneprinc) # only 399 missing :)

# C = conforme, S = ok, D = non-confome (valeur derogatoires), N = non-conforme,
# table(displv$plvconformitereferencebact)
# table(displv$plvconformitebacterio)
# table(displv$plvconformitechimique)
# table(displv$plvconformitereferencebact)

## 2B. file with measures for each parameter (heavy : 2Gb)
disres <- data.table::fread(
  file.path(data_folder, "DIS_RESULT_2025.txt"),
  sep = ",",
  header = TRUE
)
dim(disres) # 12614399

# select only nitrates
# table(disres$cdparametresiseeaux)
# param <- table(disres$libmajparametre)
# param[grep("^NITR", names(param))]
# NITRATES (EN NO3), NITRATES/50 + NITRITES/3, NITRITES (EN NO2)?
# different measures and different thresholds so keep only NO3
nitres <- disres[disres$libmajparametre %in% "NITRATES (EN NO3)", ]

# remove NAs
nitres <- nitres[!is.na(nitres$valtraduite), ]

# measure is in valtraduite
# boxplot(nitres$valtraduite)

# calculate the nitrate average per referenceprel
no3 <- tapply(nitres$valtraduite, nitres$referenceprel, mean)
m0 <- match(names(no3), displv$referenceprel)
nit_prel <- data.frame(
  "referenceprel" = names(no3),
  "mean_no3" = as.numeric(no3),
  "insee_com" = displv$inseecommuneprinc[m0],
  "year" = substr(displv$dateprel[m0], 1, 4),
  "conformite_chimique" = displv$plvconformitechimique[m0]
)

# table(nit$year)
# boxplot(nit$mean_no3 ~ nit$conformite_chimique)
# table(nit_prel$insee_com %in% commune$INSEE_COM)
# table(commune$INSEE_COM %in% nit_prel$insee_com) #2/3
# table(is.na(nit_prel$mean_no3), nit_prel$mean_no3 < 0)

# calculate summary per commune
nprel <- table(nit_prel$insee_com)
#fmt:skip
nit_com <- data.frame(
  "INSEE_COM" = names(nprel),
  "NO3_Nsamples_2025" = as.numeric(nprel),
  "NO3_mg_per_l_2025" = tapply(nit_prel$mean_no3, nit_prel$insee_com, mean)
)

# 3. export --------------------------------------------------
## 3A. per commune
m0 <- match(commune$INSEE_COM, nit_com$INSEE_COM)

keepC <- names(nit_com)[!names(nit_com) %in% names(commune)]
# plot(intM)

commune[, keepC] <- nit_com[m0, keepC]

for (i in keepC) {
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
  file.path(ind_folder, "COMMUNE_NITRATE_2025.csv"),
  row.names = FALSE
)


## 3B. per maille

## per maille 10km
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
  cross$n <- commune$NO3_Nsamples_2025[m0]
  cross$mg <- commune$NO3_mg_per_l_2025[m0]

  # weighted average
  sumN <- tapply(cross$n * cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)
  sumMG <- tapply(cross$mg * cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)
  area <- tapply(cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)

  m1 <- match(mailles$cd_sig, names(area))

  mailles$NO3_Nsamples_2025 <- (sumN / area)[m1]
  mailles$NO3_mg_per_l_2025 <- (sumMG / area)[m1]
  mailles$NO3_mg_per_l_2025[mailles$NO3_Nsamples_2025 == 0] <- NA

  # weighted average
  # cross_nb <- t(cross) * commune$NO3_Nsamples_2025
  # sum_nb <- apply(cross_nb, 2, sum, na.rm = TRUE)
  # mailles$NO3_Nsamples_2025 <- sum_nb / mailles$AREA_HA
  # # table(mailles$NO3_N_samples_2025 == 0) # 132

  # cross_mean <- t(cross) * commune$NO3_mg_per_l_2025
  # sum_mean <- apply(cross_mean, 2, sum, na.rm = TRUE)
  # mailles$NO3_mg_per_l_2025 <- sum_mean / mailles$AREA_HA

  for (j in keepC) {
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
    file.path(ind_folder, paste0("MAILLE", i, "km_NITRATE_2025.csv")),
    row.names = FALSE
  )
}
# Checkout possible issues ----------------

# check which commune in EauPotable
commune$indb <- commune$INSEE_COM %in% displv$inseecommuneprinc
png(
  file = file.path(fig_folder, "CHECK_EauPotable_in_displv.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "indb",
  border = NA,
  main = "In EauPotable"
)
dev.off()

# check which commune with NO3 measures
m0 <- match(nitres$referenceprel, displv$referenceprel)
nitres$insee_com <- displv$inseecommuneprinc[m0]
commune$inno3 <- commune$INSEE_COM %in% nitres$insee_com

png(
  file = file.path(fig_folder, "CHECK_EauPotable_with_no3.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "inno3",
  border = NA,
  main = "EauPotable with NO3"
)
dev.off()
