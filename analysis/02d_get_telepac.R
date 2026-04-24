# Script to get indicators from Telepac database

# input:
# Telepac 2024 from https://www.telepac.agriculture.gouv.fr/telepac/tbp/accueil/accueil.action
#   Liste_beneficiaires_PAC_2022_20241218.csv
# output:
#   PS_GREEN_SUBS_2022

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(data.table) #for fast processing of large files
library(terra)


data_folder <- here::here("data", "raw-data", "telepac")
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
  "code" = check_postalcode(commune$POSTAL_CODE),
  "id" = commune$INSEE_COM
)

# 2. Load and clean HVE data ------------------------------------

## 2A. file for 2022
telepac <- fread(
  file.path(data_folder, "Liste_beneficiaires_PAC_2022_20241218.csv"),
  encoding = 'Latin-1'
)

names(telepac) <- c("Nom", "Commune", "CP", "Rubrique", "Montant")

# table(telepac$Rubrique)
keepR <- c(
  "II.4 - Soutien pour les pratiques respectant le verdissement",
  "IV/15. - Soutien agroenvironnement-climat",
  "IV/16. - Aides à l'agriculture biologique",
  "IV/17. - Aides au titre de Natura 2000 et de la directive eau"
)

# select only the relevant rubrique
telepac <- telepac[Rubrique %in% keepR]

# check if there are duplicates
# table(duplicated(telepac))
# only 244 out of 367848 : negligeable
telepac <- telepac[!duplicated(telepac), ]

# remove rows with values <0 (not sure what that means)
# table(telepac$Montant < 0, useNA = "ifany") # 5710 (1.6%)
telepac <- telepac[telepac$Montant > 0, ]

# clean names and postal code
telepac$name = clean_city_names(telepac$Commune)
telepac$code = check_postalcode(telepac$CP)

telepac$incomplete_cp <- grepl("X", telepac$code)
telepac$code <- gsub("X", "0", telepac$code)
telepac$code <- as.numeric(telepac$code)

# Remove DOM-TOM
# table(telepac$code < 97000) # 2118
telepac <- telepac[telepac$code < 97000, ]

# simple match
# set the incomplete code to NA
telepac$code[telepac$incomplete_cp] = NA
m0 <- simple_match_cities(telepac, ref)
# table(is.na(m0)) # 63898 missing (17%)

# first match with a large distance to capture most synonyms (dmax=0.25)
# m1 <- match_cities(
#   telepac,
#   ref,
#   dfsyn = synonyms,
#   dmax = 0.25,
#   file.out = file.path(out_folder, "fuzzy_Annuaire_Telepac.csv")
# )
# Number of simple match: 32705(93.22%)
# Number of verified synonyms: 1803(5.14%)
# Number of fuzzy match: 550(1.57%)
# Number of non-matching elements: 27(0.08%)

# Once verified, load the additional list of synonyms
checked_fuzzy <- read.csv(file.path(
  out_folder,
  "verified_Annuaire_Telepac.csv"
))
keepR <- checked_fuzzy$verified == "ok" & !is.na(checked_fuzzy$ref_id)
checked_fuzzy <- checked_fuzzy[keepR, ] # 551 rows
dfsyn <- data.frame(
  "name" = checked_fuzzy$ori_name,
  "code" = checked_fuzzy$ori_code,
  "id" = checked_fuzzy$ref_id
)

# merge with the synonmys from INSEE and Poste
newsynonyms <- rbind(synonyms, dfsyn)

# Step 4: Final match with verified synonyms and no fuzzy match ----------------
# make match without any fuzzy match : dmax=0 and no output
m2 <- match_cities(telepac, ref, newsynonyms, file.out = NULL, dmax = 0)
# Number of simple match: 32687(93.24%)
# Number of verified synonyms: 2300(6.56%)
# Number of non-matching elements: 69(0.2%)

telepac$INSEE_COM <- m2$ref_id

# number of PAC subsidies per commune
NB_subs <- tapply(telepac$Montant > 0, telepac$INSEE_COM, sum, na.rm = TRUE)
TOT_subs <- tapply(telepac$Montant, telepac$INSEE_COM, sum, na.rm = TRUE)

# export --------------------------------------------------
## per commune
m0 <- match(commune$INSEE_COM, names(NB_subs))

commune$NB_GREENSUBS_2022 <- as.numeric(NB_subs)[m0]
commune$NB_GREENSUBS_2022[is.na(commune$NB_GREENSUBS_2022)] <- 0

commune$TOT_GREENSUBS_2022 <- as.numeric(TOT_subs)[m0]
commune$TOT_GREENSUBS_2022[is.na(commune$TOT_GREENSUBS_2022)] <- 0

png(
  file = file.path(fig_folder, "TOT_GREENSUBS_2022_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "TOT_GREENSUBS_2022",
  border = NA,
  breaks = c(0, 1, 5, 10, 100, 1000, 10000) * 1000,
  main = "Amount of green subsidies - 2022 - Commune",
  mar = c(1.1, 2.1, 1.1, 7.1)
)
dev.off()


write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_GREENSUBS_2022.csv"),
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
cross_nb <- t(cross) * commune$NB_GREENSUBS_2022
sum_nb <- apply(cross_nb, 2, sum, na.rm = TRUE)
mailles$NB_GREENSUBS_2022 <- sum_nb / mailles$AREA_HA

cross_tot <- t(cross) * commune$TOT_GREENSUBS_2022
sum_tot <- apply(cross_tot, 2, sum, na.rm = TRUE)
mailles$TOT_GREENSUBS_2022 <- sum_tot / mailles$AREA_HA

# boxplot(mailles$TOT_GREENSUBS_2022)

png(
  file = file.path(fig_folder, "TOT_GREENSUBS_2022_MAILLE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  mailles,
  y = "TOT_GREENSUBS_2022",
  border = NA,
  breaks = c(0, 1, 5, 10, 100, 1000, 10000) * 1000,
  main = "Amount of green subsidies - 2022 - Maille",
  mar = c(1.1, 2.1, 1.1, 7.1)
)
dev.off()

write.csv(
  data.frame(mailles),
  file.path(ind_folder, "MAILLE_GREENSUBS_2022.csv"),
  row.names = FALSE
)
