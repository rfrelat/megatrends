# Script to clean and project the reference spatial data
# add the postal code to the INSEE code
#
# input: (raw-data/ref/)
#   COMMUNE.shp
#     from "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
#     https://www.data.gouv.fr/datasets/code-officiel-geographique-cog
#   mailles_10km.shp
#     from https://www.patrinat.fr/fr/page-temporaire-de-telechargement-des-referentiels-de-donnees-lies-linpn-7353
# output: (derived-data/ref/)
#   commune_2154.gpkg
#   commune_4326.gpkg
#   france_4326.gpkg
#   mailles_10km_4326.gpkg
#   mailles_10km_2154.gpkg

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra) #for fast processing of large files

ref_folder <- here::here("data", "raw-data", "ref")
out_folder <- here::here("data", "derived-data", "ref")

# 2. Load and project commune ------------------------------------
metropole <- st_bbox(
  c(xmin = -9.86, xmax = 10.38, ymin = 41.15, ymax = 51.56),
  crs = st_crs(4326)
)
# ADMINEXPRESS-COG.2024:commune (11 min download)
# only has the INSEE code: not cool ...

# "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
commune <- get_wfs(
  x = st_as_sfc(metropole),
  layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
)
commune <- vect(commune)
# 8 min download

# select relevant column and rename them
keepC <- c(
  "nom_officiel_en_majuscules" = "NOM_M",
  "code_insee" = "INSEE_COM",
  "code_insee_du_departement" = "INSEE_DEP",
  "code_insee_de_la_region" = "INSEE_REG",
  "code_siren" = "SIREN_CODE",
  "code_postal" = "POSTAL_CODE",
  "population" = "POPULATION",
  "superficie_cadastrale" = "AREA_CADASTRE"
)
commune <- commune[, names(keepC)]
names(commune) = keepC
commune$AREA_HA <- expanse(commune) * 0.0001

# export as gpkg file
writeVector(commune, file.path(out_folder, "commune_4326.gpkg"))

# project to 2154 and export it
com2154 <- project(commune, "EPSG:2154")
writeVector(com2154, file.path(out_folder, "commune_2154.gpkg"))


france <- aggregate(commune) |> simplifyGeom(tolerance = 0.001)
writeVector(france, file.path(out_folder, "france_4326.gpkg"))


# 3. Load, crop and project mailles_10km ------------------------
# france <- vect(file.path(ref_folder, "france_4326.gpkg"))
maille <- vect(file.path(ref_folder, "mailles_10km.shp"))
maille_fr <- maille[maille$territoire == "METROP", ]

# much faster with simplified polygons of france
maille_fr <- crop(maille_fr, france)
maille_fr$AREA_HA <- expanse(maille_fr) * 0.0001

writeVector(maille_fr, file.path(out_folder, "mailles_10km_4326.gpkg"))

maille_2154 <- project(maille_fr, "EPSG:2154")
writeVector(maille_2154, file.path(out_folder, "mailles_10km_2154.gpkg"))
