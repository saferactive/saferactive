# from https://github.com/Robinlovelace/geocompr/blob/master/coffeeApp/app.R
library(sf)
library(shiny)
library(spData)
library(leaflet)
library(tidyverse)
world_coffee = left_join(world, coffee_data)
pal = colorNumeric(palette = "RdYlBu", domain = c(0, 4000))
lng = NULL

ui = function(request) {
  fluidPage(
    sidebarPanel(
      textOutput(outputId = "text"),
      sliderInput("range", "Coffee Production", 0, 4000,
                  value = c(1000, 4000), step = 100),
      selectInput("year", "Year", c(2016, 2017)),
      checkboxInput("legend", "Show legend", FALSE),
      textInput("lng", "Longitude", value = -10),
      textInput("lat", "Latitude", value = 0),
      textInput("zoom", "Zoom", value = 2),
      bookmarkButton()
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
}
server = function(input, output, session) {

  # # See https://shiny.rstudio.com/articles/advanced-bookmarking.html
  # # Save extra values in state$values when we bookmark
  # onBookmark(function(state) {
  #   # state$values$lng <- lng
  #   # state$values$lat <- lat
  # })
  #
  # # Read values from state$values when we restore
  # onRestore(function(state) {
  #   lng <- state$values$lng
  #   lat <- state$values$lat
  # })

  if(is.null(lng)) {
    map_centre = st_centroid(world %>% filter(name_long == "Brazil")) %>%
      st_coordinates()
  } else {
    map_centre = as.matrix(data.frame(X = lng, Y = lat))
  }

  # This reactive expression returns a character string representing the selected variable
  yr = reactive({
    paste0("coffee_production_", input$year)
  })

  # Reactive expression for the data subset to what the user selected
  filteredData = reactive({
    world_coffee$Production = world_coffee[[yr()]]
    filter(world_coffee, Production >= input$range[1] &
             Production <= input$range[2])
  })

  output$map = renderLeaflet({
    # Things that do not change go here:
    leaflet() %>% addTiles() %>%
      setView(lng = input$lng, input$lat, zoom = input$zoom)
  })

  # Changes to the map performed in an observer
  observe({
    proxy = leafletProxy("map", data = filteredData()) %>%
      clearShapes()
    # Show or hide legend
    proxy %>% clearControls() %>% addPolygons(fillColor = ~pal(Production))
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Production)
    }
  })

  observeEvent(input$map_zoom, {
    updateTextInput(session, inputId = "zoom", value = input$map_zoom)
  })

  observeEvent(input$map_center, {
    updateTextInput(session, inputId = "lat", value = round(input$map_center$lat, digits = 4))
    updateTextInput(session, inputId = "lng", value = round(input$map_center$lng, digits = 4))
  })

  output$text = renderText(input$map_zoom)

}

shinyApp(ui, server, enableBookmarking = "url")
