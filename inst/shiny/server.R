server <- function(input, output, session) {

  # Load necessary libraries
  library(dplyr)
  library(leaflet)
  library(DT)
  library(sf)

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
    showNotification("Calculating Green Index...", type = "message", duration = NULL, id = "green_index")
    index <- calculate_green_index(osm_data(), input$crs_code, input$D, input$buffer_distance)
    showNotification("Green Index calculated.", type = "message", duration = 5, id = "green_index")
    return(index)
  })

  output$green_index_table <- DT::renderDataTable({
    index <- green_index()
    if (!is.null(index)) {
      DT::datatable(index %>% select(osm_id, green_index_green_area, green_index_tree, green_index),
                    options = list(pageLength = 25))
    }
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste("green_index_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      index <- green_index()
      if (!is.null(index)) {
        write.csv(index %>% select(osm_id, green_index_green_area, green_index_tree, green_index), file)
      }
    }
  )

  output$download_json <- downloadHandler(
    filename = function() {
      paste("green_index_data", Sys.Date(), ".geojson", sep = "")
    },
    content = function(file) {
      index <- green_index()
      if (!is.null(index)) {
        sf::st_write(index, file)
      }
    }
  )

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

  # GVI Calculation and Visualization
  gvi_result <- eventReactive(input$gvi_button, {
    req(input$image_path)
    showNotification("Calculating Green View Index (GVI)...", type = "message")
    result <- calculate_and_visualize_GVI(input$image_path$datapath)
    showNotification("GVI calculated.", type = "message")
    return(result)
  })

  output$gvi_segmented_image <- renderPlot({
    result <- gvi_result()
    if (!is.null(result)) {
      OpenImageR::imageShow(result$segmented_image)
    }
  })

  output$gvi_green_pixels_image <- renderPlot({
    result <- gvi_result()
    if (!is.null(result)) {
      green_pixels_raster <- as.raster(result$green_pixels_image)
      plot(green_pixels_raster)
    }
  })

  output$gvi_value <- renderPrint({
    result <- gvi_result()
    if (!is.null(result)) {
      print(paste("Green View Index: ", result$GVI))
    }
  })

  output$download_segmented_image <- downloadHandler(
    filename = function() {
      paste("segmented_image", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      result <- gvi_result()
      if (!is.null(result)) {
        OpenImageR::writeImage(result$segmented_image, file)
      }
    }
  )

  output$download_green_pixels_image <- downloadHandler(
    filename = function() {
      paste("green_pixels_image", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      result <- gvi_result()
      if (!is.null(result)) {
        OpenImageR::writeImage(result$green_pixels_image, file)
      }
    }
  )

  output$download_html <- downloadHandler(
    filename = function() {
      paste("green_index_map", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      index <- green_index()
      if (!is.null(index)) {
        save_as_leaflet(index, file)
      }
    }
  )
}
