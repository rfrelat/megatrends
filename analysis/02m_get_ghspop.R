# Script to get the population density from GHS-POP
#
# input:
#   GHS-POP data from https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
#   ghs/GHS_POP_E2025_FRANCE_R2023A_4326_3ss_V1.tif
#
#   Schiavina M., Freire S., Carioli A., MacManus K. (2023):
#    GHS-POP R2023A - GHS population grid multitemporal (1975-2030).European Commission, Joint Research Centre (JRC)
#    PID: http://data.europa.eu/89h/2ff68a52-5b5b-4a22-8f40-c41da8332cfe, doi:10.2905/2FF68A52-5B5B-4A22-8F40-C41DA8332CFE
#   Pesaresi M., Schiavina M., Politis P., Freire S., Krasnodębska K., Uhl J. H., Carioli A., Corbane C., Dijkstra L., Florio P., Friedrich H. K., Gao J., Leyk S., Lu L., Maffenini L., Mari-Rivero I., Melchiorri M., Syrris V., Van Den Hoek J., Kemper T.
#    Advances on the Global Human Settlement Layer by joint assessment of Earth Observation and population survey data, International Journal of Digital Earth 17 (1), 2024 10.1080/17538947.2024.2390454

# output:
#   indicators_csv/MAILLEXkm_SLOPE.csv
#   indicators_csv/COMMUNE_SLOPE.csv
#   figure/SLOPE_PERC_MAILLEXkm.png
#   figure/SLOPE_PERC_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
data_folder <- here::here("data", "raw-data", "ghs")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

scales <- c("commune", "mailles_10km", "mailles_5km", "mailles_1km")

# 2. Load slope data ------------------------------------

# combine tiles

# files <- list.files(data_folder, "tif$")
# rlist <- lapply(file.path(data_folder, files), rast)
# mos <- mosaic(sprc(rlist))

# # plot(mos)
# # france <- vect(file.path(ref_folder, "france_4326.gpkg"))
# # plot(france, add = TRUE)

# writeRaster(
#   mos,
#   file.path(data_folder, "GHS_POP_E2025_FRANCE_R2023A_4326_3ss_V1.tif")
# )

pop <- rast(file.path(
  data_folder,
  "GHS_POP_E2025_FRANCE_R2023A_4326_3ss_V1.tif"
))

# 3. Overlay and calculate statistics -----------------------------
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

  exti <- exactextractr::exact_extract(
    pop,
    sf::st_as_sf(shp),
    fun = "sum"
  )
  # cor(shp$POPULATION, exti) : 0.997 high correlation with insee data
  # keep population estimate ?
  # shp$POP_N <- round(exti, 2)
  shp$POPDENSITY_N_PER_KM2_2025 <- round(exti / shp$AREA_HA * 100, 2)

  fi <- paste0("POPDENSITY_N_PER_KM2_2025_", labi, ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "POPDENSITY_N_PER_KM2_2025",
    border = NA,
    main = paste("Population density (N/km2) -", i),
    breaks = 6,
    breakby = "cases"
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_POPDENSITY_2025.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}
