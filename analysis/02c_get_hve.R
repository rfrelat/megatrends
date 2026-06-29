# Script to get indicators from HVE database

# input:
# HVE 2025 from https://www.data.gouv.fr/datasets/annuaire-des-exploitations-certifiees-haute-valeur-environnementale
#   Annuaire des exploitations HVE_Juillet 2024.xlsx
# output:
#   indicators_csv/MAILLEXkm_HVE_N_2024.csv
#   indicators_csv/COMMUNE_HVE_N_2024.csv
#   figure/HVE_N_2024_MAILLEXkm.png
#   figure/HVE_N_2024_COMMUNE.png

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
scales <- c(10, 5, 1) # in km, resolution of the mailles
# mailles <- terra::vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
# cross <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))

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
#   file.out = file.path(out_folder, "fuzzy_Annuaire_HVE.csv")
# )
# Number of simple match: 7965(87.31%)
# Number of verified synonyms: 345(3.78%)
# Number of fuzzy match: 774(8.48%)
# Number of non-matching elements: 39(0.43%)

# Once verified, load the additional list of synonyms
checked_fuzzy <- read.csv(file.path(out_folder, "verified_Annuaire_HVE.csv"))
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

commune$HVE_N_2024 <- as.numeric(n_hve)[m0]
commune$HVE_N_2024[is.na(commune$HVE_N_2024)] <- 0
# boxplot(commune$HVE_N_2024)

png(
  file = file.path(fig_folder, "HVE_N_2024_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "HVE_N_2024",
  border = NA,
  breaks = 6,
  breakby = "cases",
  main = "Number of HVE - 2024 - Commune",
)
dev.off()

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_HVE_N_2024.csv"),
  row.names = FALSE
)

## per maille
for (i in scales) {
  cat(paste("Maille", i, "km \n"))
  # load the data
  mailles <- terra::vect(
    file.path(ref_folder, paste0("mailles_", i, "km_4326.gpkg"))
  )
  cross <- readRDS(
    file.path(ref_folder, paste0("cross_mailles", i, "km_commune.rds"))
  )

  m0 <- match(cross$INSEE_COM, commune$INSEE_COM)
  cross$HVE <- commune$HVE_N_2024[m0]

  wsum <- tapply(cross$HVE * cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)
  area <- tapply(cross$AREA_HA, cross$cd_sig, sum, na.rm = TRUE)

  m1 <- match(mailles$cd_sig, names(wsum))
  # plot(area[m1], mailles$AREA_HA)
  mailles$HVE_N_2024 <- (wsum / area)[m1]

  png(
    file = file.path(fig_folder, paste0("HVE_N_2024_MAILLE", i, "km.png")),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    mailles,
    y = "HVE_N_2024",
    border = NA,
    breaks = 6,
    breakby = "cases",
    main = paste("Number of HVE - 2024 - Maille", i, "km")
  )
  dev.off()

  write.csv(
    data.frame(mailles),
    file.path(ind_folder, paste0("MAILLE", i, "km_HVE_N_2024.csv")),
    row.names = FALSE
  )
}
