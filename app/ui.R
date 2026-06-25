fluidPage(
       # run bslib::bs_theme_preview() to customize
       # theme = bs_theme(preset = "cosmo"),

       # Application title
       titlePanel("Megatrend exploration. MOTIVER, FRB-CESAB"),
       tabsetPanel(
              id = 'main',
              tabPanel(
                     title = "Maps",
                     fluidRow(
                            column(
                                   1
                            ),
                            column(
                                   4,
                                   selectInput(
                                          "var",
                                          "Variable:",
                                          choices = var_choices
                                   )
                            ),
                            column(
                                   3,
                                   selectInput(
                                          "spatial",
                                          "Spatial scale:",
                                          choices = spatial_scale
                                   )
                            ),
                            column(
                                   3,
                                   selectInput(
                                          "color",
                                          "Color scale:",
                                          choices = color_scale
                                   ),
                            )
                     ),
                     fluidRow(
                            shinycssloaders::withSpinner(
                                   leafgl::leafglOutput(
                                          "mapvar",
                                          height = 750
                                   ),
                            )
                     )
              ),
              tabPanel(
                     title = "Metrics",
                     DT::DTOutput('tableMeta'),
              ),
              tabPanel(
                     title = "About",
                     htmltools::includeMarkdown("about.md")
              ),
       )
)
