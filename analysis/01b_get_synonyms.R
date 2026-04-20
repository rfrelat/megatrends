# Script to get synonyms from city names in France
#
# input: (raw-data/ref/)
#   commune_4326.gpkg
#     from 01a_conv_ref.R
#   Poste_019HexaSmal_20250208.csv
#     from https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
#   v_commune_depuis_1943.csv
#     from https://www.data.gouv.fr/datasets/code-officiel-geographique-cog
#   v_mvt_commune_2026.csv
#     from https://www.data.gouv.fr/datasets/code-officiel-geographique-cog
# output: (derived-data/ref/)
#   commune_synonyms.csv

# 1. Load and set parameters -------------------------------------
devtools::load_all()

ref_folder <- here::here("data", "raw-data", "ref")
out_folder <- here::here("data", "derived-data", "ref")

commune <- terra::vect(file.path(out_folder, "commune_4326.gpkg"))

dfref <- data.frame(
  "name" = commune$NOM_M,
  "code" = commune$POSTAL_CODE,
  "insee_com" = commune$INSEE_COM
)


# 2. Get synonyms from La Poste -----------------------
# https://www.data.gouv.fr/fr/datasets/base-officielle-des-codes-postaux/
poste <- read.csv(
  file.path(ref_folder, "Poste_019HexaSmal_20250208.csv"),
  sep = ";",
  encoding = "latin1"
)

# select only rows in INSEE_COM
poste <- poste[poste$Code_commune_INSEE %in% commune$INSEE_COM, ]

# select row if libellé different from name
synR1 <- which(poste$Libellé_d_acheminement != poste$Nom_de_la_commune)
# select row if other comment
synR2 <- which(poste$Ligne_5 != "")
# large dataset
dfposte <- data.frame(
  "name" = c(
    poste$Nom_de_la_commune,
    poste$Libellé_d_acheminement[synR1],
    poste$Ligne_5[synR2]
  ),
  "code" = poste$Code_postal[c(1:nrow(poste), synR1, synR2)],
  "id" = poste$Code_commune_INSEE[c(1:nrow(poste), synR1, synR2)]
)

#remove duplicates
dfposte <- dfposte[!duplicated(dfposte), ]

rem <- simple_match_cities(dfposte, dfref)
# remove the simple match with the reference list
dfposte <- dfposte[is.na(rem), ]

# 3. Get synonyms from INSEE -----------------------
# https://www.data.gouv.fr/api/1/datasets/r/eb836ef4-8be2-4a55-bfdb-eb1ad041933e (last version)
# https://www.insee.fr/fr/statistiques/fichier/7766585/v_commune_depuis_1943.csv (old version)
com_insee <- read.csv(file.path(ref_folder, "v_commune_depuis_1943.csv"))

# add postal code
com_insee$Code_postal <- poste$Code_postal[
  match(com_insee$COM, poste$Code_commune_INSEE)
]

com_insee <- com_insee[
  !is.na(com_insee$Code_postal) & com_insee$COM %in% commune$INSEE_COM,
]

dfinsee <- data.frame(
  "name" = com_insee$NCC,
  "code" = com_insee$Code_postal,
  "id" = com_insee$COM
)

#remove duplicates
dfinsee <- dfinsee[!duplicated(dfinsee), ]

rem <- simple_match_cities(dfinsee, dfref)
# remove the simple match with the reference list
dfinsee <- dfinsee[is.na(rem), ]

# 4. Get synonyms from INSEE movement database -----------------------
# https://www.data.gouv.fr/api/1/datasets/r/1b96750d-08df-433b-a2d1-a8ea7545ece3 (last update)
# https://www.insee.fr/fr/statistiques/fichier/7766585/v_mvt_commune_2024.csv (older version)
mvt_insee <- read.csv(file.path(ref_folder, "v_mvt_commune_2026.csv"))

# add postal code
mvt_insee$Code_postal <- ifelse(
  is.na(match(mvt_insee$COM_AP, poste$Code_commune_INSEE)),
  poste$Code_postal[match(mvt_insee$COM_AV, poste$Code_commune_INSEE)],
  poste$Code_postal[match(mvt_insee$COM_AP, poste$Code_commune_INSEE)]
)

#fmt: skip
in_commune <- mvt_insee$COM_AV %in% commune$INSEE_COM |  mvt_insee$COM_AP %in% commune$INSEE_COM

mvt_insee <- mvt_insee[!is.na(mvt_insee$Code_postal) & in_commune, ]

mvt_insee$COM_REF <- ifelse(
  mvt_insee$COM_AP %in% commune$INSEE_COM,
  mvt_insee$COM_AP,
  mvt_insee$COM_AV
)
dfmvt <- data.frame(
  "name" = c(mvt_insee$NCC_AV, mvt_insee$NCC_AP),
  "code" = rep(mvt_insee$Code_postal, 2),
  "id" = rep(mvt_insee$COM_REF, 2)
)

#remove duplicates
dfmvt <- dfmvt[!duplicated(dfmvt), ]

rem <- simple_match_cities(dfmvt, dfref)
# remove the simple match with the reference list
dfmvt <- dfmvt[is.na(rem), ]

# 5. Merge and export -------------------------------------------
# merge the three list of synonyms
dfsyn <- rbind(dfposte, dfinsee, dfmvt)
dfsyn <- dfsyn[!duplicated(dfsyn), ]
dim(dfsyn) # 12463 synonyms

write.csv(
  dfsyn,
  file = file.path(out_folder, "commune_synonyms.csv"),
  row.names = FALSE
)
