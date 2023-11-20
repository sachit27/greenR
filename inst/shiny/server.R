library(shiny)
library(leaflet)
library(DT)
library(sf)
library(osrm)

# Assuming you have other required libraries and functions like get_osm_data, calculate_green_index, etc.

server <- function(input, output, session) {

  # Reactive expression for OSM data
  osm_data <- eventReactive(input$osm_button, {
    showNotification("Downloading OSM data...", type = "message")
    data <- get_osm_data(input$bbox)
    showNotification("OSM data downloaded.", type = "message")
    return(data)
  })

  output$osm_plot <- renderPlot({
    data <- osm_data()
    if (!is.null(data)) {
      plot(data$highways$osm_lines$geometry)
    }
  })

  # Green Index calculation and rendering
  green_index <- eventReactive(input$green_index_button, {
    showNotification("Calculating Green Index...", type = "message")
    index <- calculate_green_index(osm_data(), input$crs_code, input$D)
    showNotification("Green Index calculated.", type = "message")
    return(index)
  })

  output$green_index_table <- DT::renderDataTable({
    index <- green_index()
    if (!is.null(index)) {
      DT::datatable(index, options = list(pageLength = 25))
    }
  })

  output$green_index_plot <- leaflet::renderLeaflet({
    index <- green_index()
    if (!is.null(index)) {
      plot_green_index(index, interactive = TRUE)
    }
  })

  output$green_index_percentage <- renderPrint({
    index <- green_index()
    if (!is.null(index)) {
      calculate_percentage(index)
    }
  })

  # Download Handlers for Green Index
  output$download_html <- downloadHandler(
    filename = function() { "green_index_map.html" },
    content = function(file) {
      index <- green_index()
      if (!is.null(index)) {
        save_as_leaflet(index, file)
      }
    }
  )

  output$download_json <- downloadHandler(
    filename = function() { "green_index_data.geojson" },
    content = function(file) {
      index <- green_index()
      if (!is.null(index)) {
        save_json(index, file)
      }
    }
  )

  # Clustering Analysis
  output$cluster_map <- renderLeaflet({
    req(input$cluster_button)
    data <- osm_data()
    if (!is.null(data) && "green_areas" %in% names(data)) {
      green_space_clustering(data$green_areas, input$num_clusters)
    } else {
      showNotification("Green areas data not available.", type = "error")
    }
  })

  # Accessibility Analysis
  output$accessibility_map <- renderLeaflet({
    req(input$accessibility_button)
    data <- osm_data()
    if (!is.null(data) && "green_areas" %in% names(data)) {
      accessibility_greenspace(data$green_areas, input$location_lat,
                               input$location_lon, input$max_walk_time)
    } else {
      showNotification("Green areas data not available.", type = "error")
    }
  })

}
