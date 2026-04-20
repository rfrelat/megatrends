# Script to get indicators from HVE database

# input:
# HVE 2025 from https://www.data.gouv.fr/datasets/annuaire-des-exploitations-certifiees-haute-valeur-environnementale
#   Annuaire des exploitations HVE_Juillet 2024.xlsx
# output:
#   PS_NB_HVE_2024

# use geocoder package, check out cool tutorial:
# https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(data.table) #for fast processing of large files
library(terra)


data_folder <- here::here("data", "raw-data", "hve")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
mailles <- terra::vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
cross <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))

synonyms <- read.csv(file.path(ref_folder, "commune_synonyms.csv"))

#format the list of reference
ref <- data.frame(
  "name" = clean_city_names(commune$NOM_M),
  "code" = check_postalcode(commune$POSTAL_CODE),
  "id" = commune$INSEE_COM
)

# 2. Load and clean HVE data ------------------------------------

## 2A. file for 2024 from Deborah
hve <- readxl::read_xlsx(
  file.path(data_folder, "Annuaire des exploitations HVE_Juillet 2024.xlsx"),
  sheet = 2,
  skip = 4,
  .name_repair = "universal"
)

# Remove DOM-TOM
hve <- hve[hve$Code.Postal < 97000, ]

# remove duplicates
hve <- hve[!duplicated(hve), ]

# clean names and postal code
hve$name = clean_city_names(hve$Commune)
hve$code = check_postalcode(hve$Code.Postal)

# simple match
# m0 <- simple_match_cities(hve, ref)
# table(is.na(m0)) # 2023 missing (<10%)

# first match with a large distance to capture most synonyms (dmax=0.25)
# m1 <- match_cities(
#   hve,
#   ref,
#   dfsyn = synonyms,
#   dmax = 0.25,
#   file.out = file.path(ref_folder, "fuzzy_Annuaire_HVE.csv")
# )
# Number of simple match: 7965(87.31%)
# Number of verified synonyms: 345(3.78%)
# Number of fuzzy match: 774(8.48%)
# Number of non-matching elements: 39(0.43%)

# Once verified, load the additional list of synonyms
checked_fuzzy <- read.csv(file.path(ref_folder, "verified_Annuaire_HVE.csv"))
keepR <- checked_fuzzy$verified == "ok" & !is.na(checked_fuzzy$ref_id)
checked_fuzzy <- checked_fuzzy[keepR, ] # 776 rows
dfsyn <- data.frame(
  "name" = checked_fuzzy$ori_name,
  "code" = checked_fuzzy$ori_code,
  "id" = checked_fuzzy$ref_id
)

# merge with the synonmys from INSEE and Poste
newsynonyms <- rbind(synonyms, dfsyn)

# Step 4: Final match with verified synonyms and no fuzzy match ----------------
# make match without any fuzzy match : dmax=0 and no output
m2 <- match_cities(hve, ref, newsynonyms, file.out = NULL, dmax = 0)
# Number of simple match: 7965(87.31%)
# Number of verified synonyms: 1088(11.93%)
# Number of non-matching elements: 70(0.77%)

hve$INSEE_COM <- m2$ref_id

n_hve <- table(hve$INSEE_COM)

# export --------------------------------------------------
## per commune
m0 <- match(commune$INSEE_COM, names(n_hve))

commune$NB_HVE_2024 <- as.numeric(n_hve)[m0]
commune$NB_HVE_2024[is.na(commune$NB_HVE_2024)] <- 0
# boxplot(commune$NB_HVE_2024)

png(
  file = file.path(fig_folder, "NB_HVE_2024_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "NB_HVE_2024",
  border = NA,
  breaks = c(0, 1, 5, 10, 50, 150),
  main = "Number of HVE - 2024 - Commune",
)
dev.off()

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_NB_HVE_2024.csv"),
  row.names = FALSE
)

## per maille 10km
# load the data
# com_csv <- read.csv(file.path(ind_folder, "COMMUNE_NB_HVE_2024.csv"))
# commune$NB_HVE_2024 <- com_csv$NB_HVE_2024
# make sure the cross match
table(row.names(cross) == mailles$cd_sig, useNA = "ifany")
table(colnames(cross) == commune$INSEE_COM, useNA = "ifany")

# weighted average
cross_nb <- t(cross) * commune$NB_HVE_2024
sum_nb <- apply(cross_nb, 2, sum, na.rm = TRUE)
mailles$NB_HVE_2024 <- sum_nb / mailles$AREA_HA

# boxplot(mailles$NB_HVE_2024)

png(
  file = file.path(fig_folder, "NB_HVE_2024_MAILLE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  mailles,
  y = "NB_HVE_2024",
  border = NA,
  breaks = c(0, 1, 5, 10, 50, 150),
  main = "Number of HVE - 2024 - Commune"
)
dev.off()

write.csv(
  data.frame(mailles),
  file.path(ind_folder, "MAILLE_NB_HVE_2024.csv"),
  row.names = FALSE
)

## Extra -------------------------------------------------
## Test geocoding
## osm find very few additional match
## arcgis has fuzzy match that are not consistent, some are really good but miss easy ones...

# library(tidygeocoder)
#
# geocode with OSM
# missed <- is.na(hve$INSEE_COM)
# missing <- hve[missed, ]
# missing$Pays = "France"

# geocheck <- tidygeocoder::geocode(
#   missing,
#   street = Adresse,
#   city = Commune,
#   state = Région,
#   county = Département,
#   postalcode = Code.Postal,
#   country = Pays,
#   method = "osm"
# )

# pt_osm <- vect(geocheck, geom = c("long", "lat"), crs = "EPSG:4326")
# com_osm <- extract(commune, pt_osm)

# hve$INSEE_COM[missed] <- com_osm$INSEE_COM

# # geocode with ArcGIS
# missed <- is.na(hve$INSEE_COM)
# prop.table(table(missed)) # 1454 missing
# missing <- hve[missed, ]
# missing$inline <- paste(
#   missing$Adresse,
#   missing$Code.Postal,
#   missing$Commune,
#   sep = ", "
# )
# geocheck_arcgis <- tidygeocoder::geocode(
#   missing,
#   address = inline,
#   method = "arcgis",
#   full_results = TRUE
# )

# # set a threshold score of 90
# th_score <- 90
# geocheck_arcgis$long[geocheck_arcgis$score <= th_score] <- NA
# geocheck_arcgis$lat[geocheck_arcgis$score <= th_score] <- NA
# pt_arcgis <- vect(geocheck_arcgis, geom = c("long", "lat"), crs = "EPSG:4326")
# com_arcgis <- extract(commune, pt_arcgis)

# hve$INSEE_COM[missed] <- com_osm$INSEE_COM
