function(input, output, session) {
  # Maps ---------------------------
  output$mapvar <- renderLeaflet({
    leaflet(maille, options = leafletOptions(minZoom = Zmin, maxZoom = Zmax)) |>
      addTiles() |>
      setView(lng = Xstart, lat = Ystart, zoom = Zstart)
  })

  # Table --------------------------
  output$tableMeta <- DT::renderDT({
    DT::datatable(
      meta[!names(meta) %in% c("Script", "Megatrend", "Manifestation")],
      rownames = FALSE
    )
  })

  ## Reactive input ----------------
  colpal <- reactive({
    req(input$var)
    if (input$spatial == "commune") {
      ind <- unlist(data.frame(commune)[, input$var])
    } else {
      ind <- unlist(data.frame(maille)[, input$var])
    }
    if (input$color == "continuous") {
      pal <- colorNumeric(
        palette = "viridis",
        domain = ind,
        na.color = "transparent"
      )
    } else {
      if (input$color == "quantiles") {
        # colorQuantile() shows % instead of values in legend
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
      pal <- colorBin(
        "viridis",
        domain = ind,
        bins = bins,
        na.color = "transparent"
      )
    }
    return(pal)
  })

  shape <- reactive({
    req(input$spatial)
    if (input$spatial == "commune") {
      shp <- commune
    } else {
      shp <- maille
    }
    return(shp)
  })

  observe({
    pal <- colpal()
    shp <- shape()
    leafletProxy("mapvar", data = shp) |>
      #clearShapes() |>
      removeGlPolygons(layerId = 'mapid') |>
      addGlPolygons(
        data = shp,
        fillColor = pal(shp[[input$var]]),
        fillOpacity = 0.7,
        popup = shp[[input$var]],
        layerId = 'mapid'
      ) |>
      clearControls() |>
      # fmt:skip
      addLegend(
        position = "bottomright",
        values = shp[[input$var]],
        pal = pal,
        title = input$var,
        opacity = 1
      )
  })
}
