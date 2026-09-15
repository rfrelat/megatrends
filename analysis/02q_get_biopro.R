# Script to get the number of professional certified as organic

# input:
# data from https://www.data.gouv.fr/datasets/professionnels-engages-en-bio
#   biopro/operateursbio.csv
#   biopro/StockEtablissement_utf8.csv
#
# output:
#   indicators_csv/MAILLEXkm_BIOPRO_N_2025.csv
#   indicators_csv/COMMUNE_BIOPRO_N_2025.csv
#   figure/BIOPRO_N_2025_MAILLEXkm.png
#   figure/BIOPRO_N_2025_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(data.table) #for fast processing of large files
library(terra)

data_folder <- here::here("data", "raw-data", "biopro")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

# pre-process is heavy (siret is 8Gb file !)
# bio <- read.csv2(file.path(data_folder, "operateursbio.csv"))
# bio$SIRET <- as.numeric(bio$SIRET)
# bio_dt <- data.table(bio)

# siret <- fread(
#   file.path(data_folder, "StockEtablissement_utf8.csv"),
#   header = TRUE,
#   encoding = "UTF-8",
#   integer64 = "numeric"
# )
# bio_full <- merge(
#   bio_dt,
#   siret,
#   by.x = "SIRET",
#   by.y = "siret",
#   all.x = TRUE,
#   all.y = FALSE
# )
# keep <- c(
#   names(bio),
#   "codePostalEtablissement",
#   "libelleCommuneEtablissement",
#   "codeCommuneEtablissement",
#   "activitePrincipaleEtablissement",
#   "activitePrincipaleNAF25Etablissement"
# )
# bio_full <- bio_full[, ..keep]
# data.table::fwrite(
#   bio_full,
#   file.path(data_folder, "operateursbio_completed.csv")
# )

bio <- read.csv(file.path(data_folder, "operateursbio_completed.csv"))
bio$INSEE_COM <- ifelse(
  bio$codeCommuneEtablissement == "",
  NA,
  bio$codeCommuneEtablissement
)
# remove if not in France Metropole
table(bio$CODE.POSTAL.SIEGE.SOCIAL <= 96000)
bio <- bio[bio$codePostalEtablissement <= 96000, ]
# table(is.na(bio$SIRET)) # 816 missing out of 88k+
# table(is.na(bio$INSEE_COM)) # 903 missing out of 88k+

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
scales <- c(10, 5, 1) # in km, resolution of the mailles

#fmt:skip
ps <- grepl("^PARIS ", bio$libelleCommuneEtablissement) | bio$libelleCommuneEtablissement == "PARIS"
bio$INSEE_COM[ps] <- commune$INSEE_COM[commune$NOM_M == "PARIS"]
#fmt:skip
ms <- grepl("^MARSEILLE ", bio$libelleCommuneEtablissement) | bio$libelleCommuneEtablissement == "MARSEILLE"
bio$INSEE_COM[ms] <- commune$INSEE_COM[commune$NOM_M == "MARSEILLE"]
#fmt:skip
ly <- grepl("^LYON ", bio$libelleCommuneEtablissement) | bio$libelleCommuneEtablissement == "LYON"
bio$INSEE_COM[ly] <- commune$INSEE_COM[commune$NOM_M == "LYON"]

# table(bio$INSEE_COM %in% commune$INSEE_COM) # 86k+ match
# bio[!bio$INSEE_COM %in% commune$INSEE_COM & !is.na(bio$SIRET), ]
bio <- bio[bio$INSEE_COM %in% commune$INSEE_COM, ]
# hard to clean and maye very messy...

# Step 4: Match with commune ----------------------
n_bio <- table(bio$INSEE_COM)

# export --------------------------------------------------
## per commune
m0 <- match(commune$INSEE_COM, names(n_bio))

commune$BIOPRO_N_2025 <- as.numeric(n_bio)[m0]
commune$BIOPRO_N_2025[is.na(commune$BIOPRO_N_2025)] <- 0
# boxplot(commune$BIOPRO_N_2025)

png(
  file = file.path(fig_folder, "BIOPRO_N_2025_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "BIOPRO_N_2025",
  border = NA,
  breaks = 6,
  breakby = "cases",
  main = "Number of Organic certified companies - 2025 - Commune",
)
dev.off()

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_BIOPRO_2025.csv"),
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
  cross$BIOPRO <- commune$BIOPRO_N_2025[m0]

  wsum <- tapply(cross$BIOPRO * cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)
  area <- tapply(cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)

  m1 <- match(mailles$cd_sig, names(wsum))
  # plot(area[m1], mailles$AREA_HA)
  mailles$BIOPRO_N_2025 <- (wsum / area)[m1] |>
    round(3)

  png(
    file = file.path(fig_folder, paste0("BIOPRO_N_2025_MAILLE", i, "km.png")),
    width = 1200,
    height = 1000,
    res = 200
  )
  lab <- paste("Organic certified enterprises - 2025 - Maille", i, "km")
  plot(
    mailles,
    y = "BIOPRO_N_2025",
    border = NA,
    breaks = 6,
    breakby = "cases",
    main = lab
  )
  dev.off()

  write.csv(
    data.frame(mailles),
    file.path(ind_folder, paste0("MAILLE", i, "km_BIOPRO_N_2025.csv")),
    row.names = FALSE
  )
}
