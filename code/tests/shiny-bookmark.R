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

  # output from bookmark url example: http://127.0.0.1:6150/?_inputs_&savestate=0&map_zoom=6&zoom=%222%22&map_center=%7B%22lng%22%3A-2.28515625%2C%22lat%22%3A37.781135851454%7D&legend=false&range=%5B1000%2C4000%5D

  setBookmarkExclude(names = c("lat", "lng", "year", "map_bounds", "north", "east", "south", "west", "legend", "range"))

  # observeEvent(input$._bookmark_, {
  #   updateTextInput(session, inputId = "zoom", value = input$map_zoom)
  #   updateTextInput(session, inputId = "lat", value = input$map_center$lat)
  #   updateTextInput(session, inputId = "lng", value = input$map_center$lng)
  # })

  observeEvent(input$savestate, {
    updateTextInput(session, inputId = "zoom", value = input$map_zoom)
    updateTextInput(session, inputId = "lat", value = input$map_center$lat)
    updateTextInput(session, inputId = "lng", value = input$map_center$lng)
  })

  observeEvent(input$map_zoom, {
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
