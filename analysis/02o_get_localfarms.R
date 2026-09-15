# Script to get the number of local farms
#  raw-data/localfarms/freaietlocal.geojson
#     from https://www.fraisetlocal.fr/carte
#  raw-data/localfarms/bienvenue_a_la_ferme_data.json
#     from https://www.bienvenue-a-la-ferme.com/fr/recherche
#  raw-data/localfarms/datatourisme-place.csv
#     from https://www.data.gouv.fr/datasets/datatourisme-la-base-nationale-des-donnees-publiques-dinformation-touristique-en-open-data
# output:
#   indicators_csv/MAILLEXkm_REFEA_2023.csv
#   indicators_csv/COMMUNE_REFEA_2023.csv
#   figure/REFEA_DIST_2023_MAILLEXkm.png
#   figure/REFEA_DIST_2023_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)
library(vegan)


data_folder <- here::here("data", "raw-data", "localfarms")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")


# 2. Load and clean local farm dataset ------------------------------------
## 2a. Frais et local
frais <- vect(file.path(data_folder, "freaietlocal.geojson"))
# table(frais$categorie)
# 8487 Point de vente, 9472 Producteurs

# table(frais$nom_de_la_plateforme)
# 9255 Bienvenue à la ferme

## 2b. Bienvenue à la ferme : too complicated
# baf <- jsonlite::read_json(
#   file.path(
#     data_folder,
#     "bienvenue_a_la_ferme_data.json"
#   )
# )

## 2c. DataTourisme
place <- read.csv(file.path(data_folder, "datatourisme-place.csv"))
# identify key labels
# n <- table(place$Classements_du_POI)
# n[order(n, decreasing = TRUE)[1:50]]
label <- c(
  "Agriculture Biologique", # 1730
  "Bienvenue à la ferme", # 2060
  "Vignobles & Découvertes", # 5739
  "Accueil Paysan" # 395
)
labs <- sapply(label, function(x) grepl(x, place$Classements_du_POI))
keep <- rowSums(labs) > 0
df <- cbind(
  place[keep, c("Nom_du_POI", "Longitude", "Latitude")],
  labs[keep, ]
)
#dim(df) # 9253
pt <- vect(
  df,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)


# 3. Overlay and calculate statistics -----------------------------
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

  # Frais et local
  sph_fl <- extract(shp, frais)

  nfl <- table(sph_fl$id)
  shp$FRAISLOCAL_N_2025 <- ifelse(shp$id %in% names(nfl), nfl[shp$id], 0)
  # SELLS
  nsell <- table(sph_fl$id[frais$categorie == "Point de vente"])
  shp$FRAISLOCAL_SHOPS_N_2025 <- ifelse(
    shp$id %in% names(nsell),
    nsell[shp$id],
    0
  )
  # PRODUCTION
  nprod <- table(sph_fl$id[frais$categorie == "Producteur"])
  shp$FRAISLOCAL_PROD_N_2025 <- ifelse(
    shp$id %in% names(nprod),
    nprod[shp$id],
    0
  )

  # Tourisme
  sph_pt <- extract(shp, pt)

  npt <- table(sph_pt$id)
  shp$AGTOURISM_N_2025 <- ifelse(shp$id %in% names(npt), npt[shp$id], 0)

  var <- c(
    "FRAISLOCAL_N_2025",
    "FRAISLOCAL_SHOPS_N_2025",
    "FRAISLOCAL_PROD_N_2025",
    "AGTOURISM_N_2025"
  )

  for (j in var) {
    fj <- paste0(j, "_", labi, ".png")
    png(
      file = file.path(fig_folder, fj),
      width = 1200,
      height = 1000,
      res = 200
    )
    plot(
      shp,
      y = j,
      border = NA,
      breaks = 6,
      main = paste(j, "-", i)
    )
    dev.off()
  }
  # export
  out_fi <- paste0(gsub("S", "", labi), "_LOCAL_2025.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
