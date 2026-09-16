# Script to get the agricultural tool sharing cooperative (CUMA: coopérative d'utilisation de matériel agricole)
#  raw-data/cuma/cuma_16092026.geojson
#     from https://link.mycuma.fr/api/cumas/search/
# output:
#   indicators_csv/MAILLEXkm_CUMA_2026.csv
#   indicators_csv/COMMUNE_CUMA_2026.csv
#   figure/CUMA_DIST_KM_2026_MAILLEXkm.png
#   figure/CUMA_DIST_KM_2026_COMMUNE.png
#
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "cuma")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load and clean CUMA dataset ------------------------------------
# Get data from API
# Prepare the HTTP request
# http_request <- httr2::request("https://link.mycuma.fr/api/cumas/search/")
# # Export the data as cuma_16092026.geojson
# http_response <- httr2::req_perform(http_request, path = file.path(data_folder, "cuma_16092026.geojson"))

cuma <- vect(file.path(data_folder, "cuma_16092026.geojson"))

# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

# Same process for commune and maille
for (i in scales) {
  print(i)
  if (i == "commune") {
    shp <- vect(file.path(ref_folder, "commune_4326.gpkg"))
    shp$id <- shp$INSEE_COM
  } else {
    shp <- vect(file.path(ref_folder, paste0(i, "_4326.gpkg")))
    shp$id <- shp$cd_sig
  }

  labi <- gsub("KM$", "km", toupper(gsub("_", "", i)))

  # Check if traning center in polygons
  shp_cuma <- is.related(shp, cuma, "intersects")

  # Compute the distance among training centers and centroids
  shp_pt <- centroids(shp)
  # takes a long time with terra
  distance_matrix <- distance(cuma, shp_pt)
  # get the nearest distance
  dist_cuma <- apply(distance_matrix, 2, min)

  # boxplot(dist_cuma ~ shp_cuma)
  shp$CUMA_DIST_KM_2026 <- ifelse(shp_cuma, 0, round(dist_cuma / 1000, 3))

  # missing Corsica
  if (i == "commune") {
    shp$CUMA_DIST_KM_2026[shp$INSEE_REG == "94"] <- NA
  } else {
    # get longitude in L93
    lon <- as.numeric(substr(
      shp$cd_sig,
      regexpr("E", shp$cd_sig) + 1,
      regexpr("N", shp$cd_sig) - 1
    ))
    # threshold to identify Corsica
    th <- ifelse(i == "mailles_10km", 110, 1100)
    shp$CUMA_DIST_KM_2026[lon > th] <- NA
  }

  fi <- paste0("CUMA_DIST_KM_2026_", labi, ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "CUMA_DIST_KM_2026",
    border = NA,
    main = paste0("Distance to machinery cooperative (km) - 2026 - ", i)
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_CUMA_2026.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
