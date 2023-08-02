library(shiny)


shinyServer(function(input, output, session) {

  osm_data <- eventReactive(input$osm_button, {
    showNotification("Downloading OSM data...", type = "message")
    data <- get_osm_data(input$bbox)
    showNotification("OSM data downloaded.", type = "message")
    return(data)
  })

  output$osm_plot <- renderPlot({
    osm_data <- osm_data()
    if (!is.null(osm_data)) {
      plot(osm_data$highways$osm_lines$geometry)
    }
  })

  green_index <- eventReactive(input$green_index_button, {
    showNotification("Calculating Green Index...", type = "message")
    index <- calculate_green_index(osm_data(), input$crs_code, input$D)
    showNotification("Green Index calculated.", type = "message")
    return(index)
  })

  output$green_index_table <- DT::renderDataTable({
    green_index <- green_index()
    if (!is.null(green_index)) {
      DT::datatable(green_index, options = list(pageLength = 25))
    }
  })

  output$green_index_plot <- renderPlot({
    green_index <- green_index()
    if (!is.null(green_index)) {
      plot_green_index(green_index)
    }
  })

  output$green_index_percentage <- renderPrint({
    green_index <- green_index()
    if (!is.null(green_index)) {
      calculate_percentage(green_index)
    }
  })

  output$download_html <- downloadHandler(
    filename = function() { "green_index_map.html" },
    content = function(file) {
      green_index <- green_index()
      if (!is.null(green_index)) {
        save_as_leaflet(green_index, file)
      }
    }
  )

  output$download_json <- downloadHandler(
    filename = function() { "green_index_data.geojson" },
    content = function(file) {
      green_index <- green_index()
      if (!is.null(green_index)) {
        save_json(green_index, file)
      }
    }
  )
})
