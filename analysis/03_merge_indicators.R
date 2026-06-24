# Script to merge all indicators together
# input:
#   indicators_csv/MAILLE_XXX.csv
#   indicators_csv/COMMUNE_XXX.csv
# output:
#   indicators_csv/Dataset_maille.csv
#   indicators_csv/Dataset_commune.csv

# Load home made functions
devtools::load_all()

ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
out_app <- here::here("app", "data")

commune <- terra::vect(file.path(ref_folder, "commune_simple0005_4326.gpkg"))
mailles <- terra::vect(file.path(ref_folder, "mailles_10km_4326.gpkg"))

meta <- readxl::read_xlsx(
  here::here("data", "derived-data", "megatrends_metadata.xlsx")
)

srch <- "^XX_.*csv$"
# Same process for commune and maille
for (i in c("commune", "maille")) {
  if (i == "commune") {
    shp <- commune
  } else {
    shp <- mailles
  }
  srchi <- gsub("XX", toupper(i), srch)
  ifiles <- list.files(ind_folder, srchi, full.names = TRUE)
  # remove EauFrance indicators
  ifiles <- ifiles[!grepl("EauFrance", ifiles)]
  # load all csv
  ind <- lapply(ifiles, read.csv)
  ind <- lapply(ind, rm_col, col = names(shp))
  ind <- do.call(cbind, ind)
  ind <- cbind(data.frame(shp), ind)

  # calculate new indicators when needed
  # percentage area of organic farming
  maxarea <- ifelse(
    ind$CULTIVATED_AREA_HA < ind$BIO_AREA_HA_2024,
    ind$BIO_AREA_HA_2024,
    ind$CULTIVATED_AREA_HA
  )

  ind$BIO_AREA_PCT_2024 <- ifelse(
    maxarea > 0,
    ind$BIO_AREA_HA_2024 / maxarea * 100,
    NA
  )

  # percentage area of organic farming
  ind$GREENSUBS_kEUR_per_HA_2022 <- ifelse(
    ind$CULTIVATED_AREA_HA > 0,
    ind$GREENSUBS_kEUR_2022 / ind$CULTIVATED_AREA_HA,
    NA
  )

  #re-order indicators
  ind <- ind[, unique(c(names(shp), meta$Name))]

  # export as csv file
  outi <- paste0("Dataset_", i, ".csv")
  write.csv(
    ind,
    file.path(ind_folder, outi),
    row.names = FALSE
  )

  # export as geopackage for shinyapp
  shp <- cbind(shp, ind[, !names(ind) %in% names(shp)])
  terra::writeVector(
    shp,
    file.path(out_app, paste0(i, ".gpkg")),
    overwrite = TRUE
  )
}
