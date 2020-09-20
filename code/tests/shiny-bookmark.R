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
      actionButton(inputId = "savestate", label = "Save map state"),
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

  observeEvent(input$._bookmark_, {
    updateTextInput(session, inputId = "zoom", value = input$map_zoom)
    updateTextInput(session, inputId = "lat", value = input$map_center$lat)
    updateTextInput(session, inputId = "lng", value = input$map_center$lng)
    session$doBookmark()
  })

  observeEvent(input$savestate, {
    updateTextInput(session, inputId = "zoom", value = input$map_zoom)
    updateTextInput(session, inputId = "lat", value = input$map_center$lat)
    updateTextInput(session, inputId = "lng", value = input$map_center$lng)
  })

  onRestore(function(state) {
    updateTextInput(session, inputId = "zoom", value = state$input$map_zoom)
    updateTextInput(session, inputId = "lat", value = state$input$map_center$lat)
    updateTextInput(session, inputId = "lng", value = state$input$map_center$lng)
  })

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

  latlng = reactive({

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

  # demonstrate updating text input of map:
  # observeEvent(input$map_zoom, {
  #   updateTextInput(session, inputId = "zoom", value = input$map_zoom)
  # })

  # # See https://shiny.rstudio.com/articles/advanced-bookmarking.html
  # # Save extra values in state$values when we bookmark
  # onBookmark(function(state) {
  #   updateTextInput(session, inputId = "zoom", value = input$map_zoom)
  #   updateTextInput(session, inputId = "lat", value = input$map_center$lat)
  #   updateTextInput(session, inputId = "lng", value = input$map_center$lng)
  # })
  #
  # Read values from state$values when we restore
  # onRestore(function(state) {
  #   updateTextInput(session, inputId = "lat", value = round(as.numeric(state$values$map_center$lat), digits = 4))
  #   updateTextInput(session, inputId = "lng", value = round(as.numeric(state$values$map_center$lng), digits = 4))
  #   # input$lng <- state$values$map_center$lng
  #   # input$lat <- state$values$map_center$lat
  # })

  output$text = renderText(input$map_zoom)

}

shinyApp(ui, server, enableBookmarking = "url")
