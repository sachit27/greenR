ui <- fluidPage(
  titlePanel(
    tags$div(
      tags$img(src = "logo.jpg", height = 80, width = 80),
      "greenR: Green Index Analysis"
    )
  ),

  sidebarLayout(
    sidebarPanel(
      helpText("This application allows you to download OSM data, calculate the green index, visualize and save the results, perform clustering of green areas, analyze green space accessibility and Green View Index."),

      # Input for OSM data
      textInput("bbox", "Enter location/bounding box"),
      numericInput("crs_code", "Enter CRS Code", value = 4326),
      helpText(a("What is CRS code?", href = "https://epsg.io/", target = "_blank")),
      numericInput("D", "Enter decay parameter 'D'", value = 100),
      numericInput("buffer_distance", "Enter buffer distance", value = 120),
      actionButton("osm_button", "Download OSM Data"),
      actionButton("green_index_button", "Calculate Green Index"),

      # Download Buttons
      downloadButton("download_csv", "Download Data as CSV"),
      downloadButton("download_json", "Download Data as GeoJSON"),
      downloadButton("download_html", "Download Green Index Map as HTML"),  # Add this line

      # Clustering Inputs
      numericInput("num_clusters", "Number of Clusters", value = 5),
      actionButton("cluster_button", "Perform Clustering"),

      # Accessibility Analysis Inputs
      numericInput("location_lat", "Latitude of Location", value = 0),
      numericInput("location_lon", "Longitude of Location", value = 0),
      numericInput("max_walk_time", "Maximum Walking Time (minutes)", value = 15),
      actionButton("accessibility_button", "Perform Accessibility Analysis"),

      # Green View Index Inputs
      fileInput("image_path", "Upload Image for GVI Calculation", accept = c("image/png", "image/jpeg")),
      actionButton("gvi_button", "Calculate GVI"),
      downloadButton("download_segmented_image", "Download Segmented Image"),
      downloadButton("download_green_pixels_image", "Download Green Pixels Image")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("OSM Data", plotOutput("osm_plot", height = "90vh")),
        tabPanel("Green Index Data",
                 DT::dataTableOutput("green_index_table"),
                 downloadButton("download_csv", "Download Data as CSV"),
                 downloadButton("download_json", "Download Data as GeoJSON")
        ),
        tabPanel("Green Index Plot", leaflet::leafletOutput("green_index_plot", height = "80vh")),
        tabPanel("Green Index Percentage", verbatimTextOutput("green_index_percentage")),
        tabPanel("Cluster Analysis", leaflet::leafletOutput("cluster_map", height = "80vh")),
        tabPanel("Accessibility Analysis", leaflet::leafletOutput("accessibility_map", height = "80vh")),
        tabPanel("Green View Index",
                 plotOutput("gvi_segmented_image", height = "30vh"),
                 plotOutput("gvi_green_pixels_image", height = "30vh"),
                 verbatimTextOutput("gvi_value")
        )
      )
    )
  )
)
