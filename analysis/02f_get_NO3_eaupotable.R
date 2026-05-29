# Script to get bitrate levels from EauPotable database

# input:
#   eaupotable/dis-2025 from
#     https://www.data.gouv.fr/datasets/resultats-du-controle-sanitaire-de-leau-distribuee-commune-par-commune
# output:
#   indicators_csv/COMMUNE_NITRATE_2025.csv
#   indicators_csv/MAILLE_NITRATE_2025.csv
#   figure/NITRATE_2025_XX_COMMUNE.png
#   figure/NITRATE_2025_XX_MAILLE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "eaupotable", "dis-2025")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
mailles <- terra::vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
# to simplify and fasten the mapping
cross <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))
# add known commune synonyms to improve the number of matches
synonyms <- read.csv(file.path(ref_folder, "commune_synonyms.csv"))

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
  "insee_com" = names(nprel),
  "NO3_N_samples" = as.numeric(nprel),
  "NO3_mean_mg_per_l" = tapply(nit_prel$mean_no3, nit_prel$insee_com, mean),
  "NO3_max_mg_per_l" = tapply(nit_prel$mean_no3, nit_prel$insee_com, max)
)


# 3. export --------------------------------------------------
## 3A. per commune
m0 <- match(commune$INSEE_COM, nit_com$insee_com)

commune$NO3_2025_N_samples <- nit_com$NO3_N_samples[m0]
commune$NO3_2025_mean_mg_per_l <- nit_com$NO3_mean_mg_per_l[m0]
commune$NO3_2025_max_mg_per_l <- nit_com$NO3_max_mg_per_l[m0]

for (i in names(commune)[-(1:9)]) {
  li <- gsub("^NO3_", "", i)
  filei <- paste0("NITRATE_", toupper(li), "_COMMUNE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(commune, y = i, border = NA, main = paste("Nitrate -", li, "- Commune"))
  dev.off()
}

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_NITRATE_2025.csv"),
  row.names = FALSE
)


## 3B. per maille 10km

# weighted average
cross_nb <- t(cross) * commune$NO3_2025_N_samples
sum_nb <- apply(cross_nb, 2, sum, na.rm = TRUE)
mailles$NO3_2025_N_samples <- sum_nb / mailles$AREA_HA
# table(mailles$NO3_N_samples_2025 == 0) # 132

cross_mean <- t(cross) * commune$NO3_2025_mean_mg_per_l
sum_mean <- apply(cross_mean, 2, sum, na.rm = TRUE)
mailles$NO3_2025_mean_mg_per_l <- sum_mean / mailles$AREA_HA
mailles$NO3_2025_mean_mg_per_l[mailles$NO3_2025_N_samples == 0] <- NA
# table(mailles$NO3_mean_mg_per_l_2025 == 0, useNA = "ifany") # 206

cross_max <- t(!is.na(cross)) * commune$NO3_2025_max_mg_per_l
mailles$NO3_2025_max_mg_per_l <- apply(cross_max, 2, max, na.rm = TRUE)
mailles$NO3_2025_max_mg_per_l[mailles$NO3_2025_N_samples == 0] <- NA
# table(mailles$NO3_2025_max_mg_per_l == 0, useNA = "ifany")

# boxplot(mailles$TOT_GREENSUBS_2022)
for (i in names(mailles)[-(1:3)]) {
  li <- gsub("^NO3_", "", i)
  filei <- paste0("NITRATE_", toupper(li), "_MAILLE.png")
  png(
    file = file.path(fig_folder, filei),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(mailles, y = i, border = NA, main = paste("Nitrate -", li, "- Maille"))
  dev.off()
}

write.csv(
  data.frame(mailles),
  file.path(ind_folder, "MAILLE_NITRATE_2025.csv"),
  row.names = FALSE
)

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
