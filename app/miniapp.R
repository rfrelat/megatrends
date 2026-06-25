source("app/global.R")

input <- list(
  spatial = spatial_scale[1],
  var = var_choices[4],
  color = color_scale[3]
)

# reactive shape()
if (input$spatial == "commune") {
  shp <- commune
} else {
  shp <- maille
}

# reactive colpal()
if (input$spatial == "commune") {
  ind <- unlist(data.frame(commune)[, input$var])
} else {
  ind <- unlist(data.frame(maille)[, input$var])
}
if (input$color == "continuous") {
  colpal <- colorNumeric(
    palette = "viridis",
    domain = ind,
    na.color = "transparent"
  )
} else {
  if (input$color == "quantiles") {
    bins <- quantile(ind, probs = seq(0, 1, 0.2), na.rm = TRUE)
  } else {
    # log scale
    range_log <- range(log1p(ind), na.rm = TRUE)
    bins <- expm1(seq(range_log[1], range_log[2], length.out = 9))
  }
  # simplify bins
  bins <- unique(c(
    floor(bins[1]),
    round(bins[2:(length(bins) - 1)], 1),
    ceiling(bins[length(bins)])
  ))
  colpal <- colorBin(
    "viridis",
    domain = ind,
    bins = bins,
    na.color = "transparent"
  )
}


# colpal <- colorQuantile(
#   palette = "viridis",
#   domain = ind,
#   n = 5,
#   na.color = "transparent"
# )

leaflet(shp, options = leafletOptions(minZoom = Zmin, maxZoom = Zmax)) |>
  addTiles() |>
  setView(lng = Xstart, lat = Ystart, zoom = Zstart) |>
  addGlPolygons(
    data = shp,
    fillColor = colpal(shp[[input$var]]),
    fillOpacity = 0.7,
    popup = shp[[input$var]],
    layerId = 'mapid'
  ) |>
  clearControls() |>
  # fmt:skip
  addLegend(
        position = "bottomright",
        values = shp[[input$var]],
        pal = colpal,
        title = input$var,
        opacity = 1
      )
