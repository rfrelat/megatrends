# Script to get nitrate indicators from EauFrance
# The dataset is full of holes so the indicators were DISCARDED
# THIS SCRIPT IS AN ARCHIVED - NOT USED FOR MEGATRENDS
#
# input:
#   nid/NID_France_2024_Data.xksx from https://rapportage.eaufrance.fr/node/194
# output:
#   indicators_csv/MAILLE_NITRATE_EauFrance_2024.csv
#   indicators_csv/COMMUNE_NITRATE_EauFrance_2024.csv
#   figure/NITRATE_EAUFRANCE_XX_MAILLE.png
#   figure/NITRATE_EAUFRANCE_XX_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "nid")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_4326.gpkg"))
mailles <- terra::vect(file.path(ref_folder, "mailles_10km_4326.gpkg"))

# 2. Load and pre-process NID data ------------------------------------

## 2A. groundwater
gw_stat <- readxl::read_xlsx(
  file.path(data_folder, "NID_France_2024_Data.xlsx"),
  sheet = "NiD_GW_Stat",
)
gw_conc <- readxl::read_xlsx(
  file.path(data_folder, "NID_France_2024_Data.xlsx"),
  sheet = "NiD_GW_AnnConc"
)
gw_conc$ND_AvgAnnValue <- as.numeric(gw_conc$ND_AvgAnnValue)
# boxplot(gw_conc$ND_AvgAnnValue) # in mg/l

m0 <- match(gw_conc$ND_NatStatCode, gw_stat$ND_NatStatCode)
gw_conc$Longitude <- as.numeric(gw_stat$Longitude[m0])
gw_conc$Latitude <- as.numeric(gw_stat$Latitude[m0])

pts_gw <- terra::vect(
  gw_conc,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

## 2B. surface water
sw_stat <- readxl::read_xlsx(
  file.path(data_folder, "NID_France_2024_Data.xlsx"),
  sheet = "NiD_SW_Stat",
)
sw_conc <- readxl::read_xlsx(
  file.path(data_folder, "NID_France_2024_Data.xlsx"),
  sheet = "NiD_SW_AnnConc"
)
sw_conc$ND_AvgAnnValue <- as.numeric(sw_conc$ND_AvgAnnValue)
# boxplot(sw_conc$ND_AvgAnnValue) # in mg/l
# only 3318 stations ...

m0 <- match(sw_conc$ND_NatStatCode, sw_stat$ND_NatStatCode)
sw_conc$Longitude <- as.numeric(sw_stat$Longitude[m0])
sw_conc$Latitude <- as.numeric(sw_stat$Latitude[m0])

pts_sw <- terra::vect(
  sw_conc,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

# 3. Overlay and calculate statistics -----------------------------

## 3a. for mailles 10km
# surface water
extM_sw <- extract(mailles, pts_sw)
# table(is.na(extM_sw$cd_sig))
sw_conc$cd_sig <- extM_sw$cd_sig
nsw <- table(sw_conc$cd_sig)
#fmt: skip
out_sw <- data.frame(
  "cd_sig" = names(nsw),
  "NO3_SW_2024_N_stations" = as.numeric(nsw),
  "NO3_SW_2024_N_samples" = tapply(sw_conc$ND_NoOfSamples_Year, sw_conc$cd_sig, sum, na.rm = TRUE),
  "NO3_SW_2024_mg_per_l" = tapply(sw_conc$ND_AvgAnnValue, sw_conc$cd_sig, mean, na.rm = TRUE)
)
m_sw <- match(mailles$cd_sig, out_sw$cd_sig)
keepC <- names(out_sw)[!names(out_sw) %in% names(mailles)]
mailles[, keepC] <- out_sw[m_sw, keepC]

# ground water
extM_gw <- extract(mailles, pts_gw)
# table(is.na(extM_gw$cd_sig))
gw_conc$cd_sig <- extM_gw$cd_sig
ngw <- table(gw_conc$cd_sig)
#fmt: skip
out_gw <- data.frame(
  "cd_sig" = names(ngw),
  "NO3_GW_2024_N_stations" = as.numeric(ngw),
  "NO3_GW_2024_N_samples" = tapply(gw_conc$ND_NoOfSamples_Year, gw_conc$cd_sig, sum, na.rm = TRUE),
  "NO3_GW_2024_mg_per_l" = tapply(gw_conc$ND_AvgAnnValue, gw_conc$cd_sig, mean, na.rm = TRUE)
)
m_gw <- match(mailles$cd_sig, out_gw$cd_sig)
keepC <- names(out_gw)[!names(out_gw) %in% names(mailles)]
mailles[, keepC] <- out_gw[m_gw, keepC]


for (i in names(mailles)[-(1:3)]) {
  li <- gsub("^NO3_", "", i)
  filei <- paste0("NITRATE_EAUFRANCE_", toupper(li), "_MAILLE.png")
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
    main = paste("Nitrate - EauFrance -", li, "- Maille")
  )
  dev.off()
}

write.csv(
  data.frame(mailles),
  file.path(ind_folder, "MAILLE_NITRATE_EauFrance_2024.csv"),
  row.names = FALSE
)


## 3b. for communes
# surface water
extC_sw <- extract(commune, pts_sw)
sw_conc$INSEE_COM <- extC_sw$INSEE_COM
nsw <- table(sw_conc$INSEE_COM)
#fmt: skip
out_sw <- data.frame(
  "INSEE_COM" = names(nsw),
  "NO3_SW_2024_N_stations" = as.numeric(nsw),
  "NO3_SW_2024_N_samples" = tapply(sw_conc$ND_NoOfSamples_Year, sw_conc$INSEE_COM, sum, na.rm = TRUE),
  "NO3_SW_2024_mg_per_l" = tapply(sw_conc$ND_AvgAnnValue, sw_conc$INSEE_COM, mean, na.rm = TRUE)
)
m_sw <- match(commune$INSEE_COM, out_sw$INSEE_COM)
keepC <- names(out_sw)[!names(out_sw) %in% names(commune)]
commune[, keepC] <- out_sw[m_sw, keepC]

# ground water
extM_gw <- extract(commune, pts_gw)
gw_conc$INSEE_COM <- extM_gw$INSEE_COM
ngw <- table(gw_conc$INSEE_COM)
#fmt: skip
out_gw <- data.frame(
  "INSEE_COM" = names(ngw),
  "NO3_GW_2024_N_stations" = as.numeric(ngw),
  "NO3_GW_2024_N_samples" = tapply(gw_conc$ND_NoOfSamples_Year, gw_conc$INSEE_COM, sum, na.rm = TRUE),
  "NO3_GW_2024_mg_per_l" = tapply(gw_conc$ND_AvgAnnValue, gw_conc$INSEE_COM, mean, na.rm = TRUE)
)
m_gw <- match(commune$INSEE_COM, out_gw$INSEE_COM)
keepC <- names(out_gw)[!names(out_gw) %in% names(commune)]
commune[, keepC] <- out_gw[m_gw, keepC]


for (i in names(commune)[-(1:9)]) {
  li <- gsub("^NO3_", "", i)
  filei <- paste0("NITRATE_EAUFRANCE_", toupper(li), "_COMMUNE.png")
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
    main = paste("Nitrate - EauFrance", li, "- Commune")
  )
  dev.off()
}

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_NITRATE_EauFrance_2024.csv"),
  row.names = FALSE
)
