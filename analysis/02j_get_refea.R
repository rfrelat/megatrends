# Script to get distance to agricultural training center
# input:
#  raw-data/masa/refea_2025-2026.csv
#     from https://enseignement-agricole.opendatasoft.com/explore/assets/refea-liste-des-etablissements-proposant-des-formations-agricoles-2025-2026/
# output:
#   indicators_csv/MAILLEXkm_REFEA_2023.csv
#   indicators_csv/COMMUNE_REFEA_2023.csv
#   figure/REFEA_DIST_2023_MAILLEXkm.png
#   figure/REFEA_DIST_2023_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)
library(vegan)


data_folder <- here::here("data", "raw-data", "masa")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")


# 2. Load and clean RPG data ------------------------------------
refea <- read.csv2(file.path(data_folder, "refea_2025-2026.csv"))

# remove unknown coordinates
# table(refea$coordonnees_geo == "") # 234 out of 5600+
# refea$Commune[refea$coordonnees_geo == ""] # mostly large cities or DOM TOM
refea <- refea[refea$coordonnees_geo != "", ]
# get coordinates
xy <- strsplit(refea$coordonnees_geo, ", ")
refea$latitude <- as.numeric(sapply(xy, function(x) x[[1]]))
refea$longitude <- as.numeric(sapply(xy, function(x) x[[2]]))
refea_pt <- vect(refea, c("longitude", "latitude"), crs = "EPSG:4326")

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
  shp_ea <- is.related(shp, refea_pt, "intersects")

  # Compute the distance among training centers and centroids
  shp_pt <- centroids(shp)
  # takes a long time with terra
  distance_matrix <- distance(refea_pt, shp_pt)
  # get the nearest distance
  dist_ea <- apply(distance_matrix, 2, min)

  # boxplot(dist_ea ~ shp_ea)
  shp$REFEA_DIST_KM_2025 <- ifelse(shp_ea, 0, round(dist_ea / 1000, 2))

  # shortest distance : faster in sf
  # whoshort <- sf::st_nearest_feature(shp_pt, refea_pt)
  # mindist <- sf::st_distance(
  #   shp_pt,
  #   refea_pt[whoshort, ],
  #   by_element = TRUE
  # )

  fi <- paste0("REFEA_DIST_KM_2025_", labi, ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "REFEA_DIST_KM_2025",
    border = NA,
    main = paste0("Distance to training (km) - 2025 - ", i)
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_REFEA_2025.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
