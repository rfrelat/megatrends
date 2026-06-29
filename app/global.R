suppressPackageStartupMessages({
  require(shiny)
  require(leaflet)
  require(leafgl)
  require(here)
  require(sf)
  require(htmltools)
  require(markdown)
  require(shinycssloaders)
})

folder <- "data"
# folder <- here::here("app", "data")

# load datasets
spatial_scale <- c("commune", "maille")

# Leaflet zoom parameter
Zmin <- 3
Zmax <- 9
Zstart <- 5
Xstart <- 2
Ystart <- 46

# type of maps
color_scale <- c("continuous", "quantiles", "log")

meta <- read.csv(
  file.path(folder, "megatrends_metadata.csv")
)

# transform to polygon instead of multipolygon
# commune <- st_read(file.path(folder, "commune.gpkg"))
# commune_poly <- sf::st_cast(commune, "POLYGON", warn = FALSE)
# sf::st_write(commune_poly, file.path(folder, "commune_poly.gpkg"))
commune <- sf::st_read(file.path(folder, "commune.gpkg"), quiet = TRUE)

# maille <- st_read(file.path(folder, "maille.gpkg"))
# maille_poly <- sf::st_cast(maille, "POLYGON", warn = FALSE)
# sf::st_write(maille_poly, file.path(folder, "maille_poly.gpkg"))
maille <- sf::st_read(file.path(folder, "mailles_10km.gpkg"), quiet = TRUE)

var_choices <- names(maille)[names(maille) %in% meta$Name]
var_choices <- var_choices[var_choices != "CROP_DOMINANT_2023"]
