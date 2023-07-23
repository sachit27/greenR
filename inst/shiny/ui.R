library(shiny)

shinyUI(fluidPage(

  titlePanel(
    tags$div(
      tags$img(src = "logo.png", height = 80, width = 80),
      "greenR: Green Index Analysis"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      helpText("This app allows you to download OSM data, calculate the green index,
                visualize and save the results."),

      textInput("bbox", "Enter location/bounding box"),

      numericInput("crs_code", "Enter CRS Code", value = 4326),
      helpText(a("What is CRS code?", href = "https://epsg.io/", target = "_blank")),

      numericInput("D", "Enter decay parameter 'D'", value = 100),

      actionButton("osm_button", "Download OSM Data"),
      actionButton("green_index_button", "Calculate Green Index"),

      downloadButton("download_html", "Download Map as HTML"),
      downloadButton("download_json", "Download Data as GeoJSON")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("OSM Data", plotOutput("osm_plot", height = "90vh")),
        tabPanel("Green Index Data", DT::dataTableOutput("green_index_table")),
        tabPanel("Green Index Plot", plotOutput("green_index_plot", height = "80vh")),
        tabPanel("Green Index Percentage", verbatimTextOutput("green_index_percentage"))
      )
    )
  )
))
