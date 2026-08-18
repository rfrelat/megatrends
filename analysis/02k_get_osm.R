# Script to get road and hiking paths density from OpenStreetMap
#
# input:
#   osm data from osmextract::oe_get("France", stringsAsFactors = FALSE) on 2026-08-18
#   osm/geofabrik_france-latest.gpkg
#
# output:
#   indicators_csv/MAILLEXkm_OSM_2026.csv
#   indicators_csv/COMMUNE_OSM_2026.csv
#   figure/OSM_XX_2026_MAILLEXkm.png
#   figure/OSM_XX_2026_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

data_folder <- here::here("data", "raw-data", "osm")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load and clean OSM data ------------------------------------
# https://docs.ropensci.org/osmextract/
osm <- vect(
  file.path(data_folder, "geofabrik_france-latest.gpkg"),
  layer = "lines"
)

# table(osm$highway)
# https://wiki.openstreetmap.org/wiki/Key:highway

#fmt:skip
road <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential",
           "motorway_link ", "trunk_link ", "primary_link ", "secondary_link ", "tertiary_link ",
           "living_street", "service", "busway")

hike <- c("footway", "bridleway", "steps", "path", "track")

osm_road <- osm[osm$highway %in% road]
osm_hike <- osm[osm$highway %in% hike]

# compare with official compilation
# https://www.data.gouv.fr/datasets/itineraires-de-randonnee-dans-openstreetmap
# osm_hike_gvt <- vect(file.path(data_folder, "hiking_foot_routes_lineLine.shp"))
# osm_hike_gvt <- project(osm_hike_gvt, "EPSG:4326")
# table(osm_hike_gvt$route)
# osm_hike_gvt$name <- NULL
# route = hiking
# route = foot
# zoom <- shp[shp$NOM_M == "FABREGUES"]
# z_hk <- crop(osm_hike, ext(zoom))
# z_rd <- crop(osm_road, ext(zoom))
# z <- crop(osm, ext(zoom))
# z_hk2 <- crop(osm_hike_gvt, ext(zoom))
# mapview::mapview(z_hk, color = "red") +
#   mapview::mapview(z_hk2, color = "blue") +
#   mapview::mapview(z_rd, color = "black")
# mapview::mapview(z, color = "black")

# OSMDATA solution: too long
# library(osmdata)
# q <- opq(getbb("fabregues"))
# z_api <- opq(getbb("fabregues")) |>
#   add_osm_feature(key = "highway", value = hike) |>
#   osmdata_sf()
# # super long ... : # HTTP 429 Too Many Requests.

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

  # hiking path
  inti_hk <- intersect(osm_hike, shp)
  inti_hk$length <- perim(inti_hk)
  hk_sumid <- tapply(inti_hk$length, inti_hk$id, sum, na.rm = TRUE)
  m_hk <- match(shp$id, names(hk_sumid))

  shp$OSM_HIKING_KM_2026 <- ifelse(
    is.na(m_hk),
    0,
    round(hk_sumid[m_hk] / 1000, 3)
  )

  shp$OSM_HIKING_M_PER_HA_2026 <- shp$OSM_HIKING_KM_2026 * 1000 / shp$AREA_HA

  # compare with OSM data provided by gouvernement
  # inti_hk2 <- intersect(osm_hike_gvt, shp)
  # inti_hk2$length <- perim(inti_hk2)
  # hk2_sumid <- tapply(inti_hk2$length, inti_hk2$id, sum, na.rm = TRUE)
  # m_hk2 <- match(shp$id, names(hk2_sumid))
  # shp$OSM_HIKING2_KM_2026 <- ifelse(
  #   is.na(m_hk2),
  #   0,
  #   round(hk2_sumid[m_hk2] / 1000, 3)
  # )
  # plot(
  #   shp,
  #   y = "OSM_HIKING2_KM_2026",
  #   border = NA,
  #   breaks = 6,
  #   breakby = "cases",
  #   main = paste(j, "-", i)
  # )
  # cor(shp$OSM_HIKING_KM_2026, shp$OSM_HIKING2_KM_2026)

  # roads
  inti_rd <- intersect(osm_road, shp)
  inti_rd$length <- perim(inti_rd)
  rd_sumid <- tapply(inti_rd$length, inti_rd$id, sum, na.rm = TRUE)
  m_rd <- match(shp$id, names(rd_sumid))

  shp$OSM_ROADS_KM_2026 <- ifelse(
    is.na(m_rd),
    0,
    round(rd_sumid[m_rd] / 1000, 3)
  )

  shp$OSM_ROADS_M_PER_HA_2026 <- shp$OSM_ROADS_KM_2026 * 1000 / shp$AREA_HA

  var <- c(
    "OSM_HIKING_KM_2026",
    "OSM_HIKING_M_PER_HA_2026",
    "OSM_ROADS_KM_2026",
    "OSM_ROADS_M_PER_HA_2026"
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
      breakby = "cases",
      main = paste(j, "-", i)
    )
    dev.off()
  }

  # export
  out_fi <- paste0(gsub("S", "", labi), "_OSM_2026.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
