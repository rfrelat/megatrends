# Script to clean and project the reference spatial data
#
# input: (raw-data/ref/)
#   COMMUNE.shp
#   mailles_10km.shp
#     from https://www.patrinat.fr/fr/page-temporaire-de-telechargement-des-referentiels-de-donnees-lies-linpn-7353
# output: (derived-data/ref/)
#   commune_2154.gpkg
#   commune_4326.gpkg
#   france_4326.gpkg
#   france_simp_4326.gpkg
#   mailles_10km_4326.gpkg
#   mailles_10km_2154.gpkg

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra) #for fast processing of large files

ref_folder <- here::here("data", "raw-data", "ref")
out_folder <- here::here("data", "derived-data", "ref")

# 2. Load and project commune ------------------------------------
commune <- vect(file.path(ref_folder, "COMMUNE.shp"))

commune$AREA_HA <- expanse(commune) * 0.0001
# select relevant column
#fmt: skip
keepC <- c("NOM_M", "INSEE_COM","INSEE_DEP", "INSEE_REG", "POPULATION","AREA_HA")
commune <- commune[, keepC]
writeVector(commune, file.path(out_folder, "commune_2154.gpkg"))

com4326 <- project(commune, "EPSG:4326")
writeVector(com4326, file.path(out_folder, "commune_4326.gpkg"))

france <- aggregate(com4326)
writeVector(france, file.path(out_folder, "france_4326.gpkg"))

france2 <- simplifyGeom(france, tolerance = 0.01)
writeVector(france2, file.path(out_folder, "france_simp_4326.gpkg"))


# 3. Load, crop and project mailles_10km ------------------------
# com4326 <- vect(file.path(ref_folder, "commune_4326.gpkg"))

maille <- vect(file.path(ref_folder, "mailles_10km.shp"))
maille_fr <- maille[maille$territoire == "METROP", ]

# much faster with simplified polygons of france
maille_fr <- crop(maille_fr, france2)
maille_fr$AREA_HA <- expanse(maille_fr) * 0.0001

writeVector(maille_fr, file.path(out_folder, "mailles_10km_4326.gpkg"))

maille_2154 <- project(maille_fr, "EPSG:2154")
writeVector(maille_2154, file.path(out_folder, "mailles_10km_2154.gpkg"))
