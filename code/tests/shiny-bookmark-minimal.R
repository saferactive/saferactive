# from https://github.com/Robinlovelace/geocompr/blob/master/coffeeApp/app.R
library(sf)
library(shiny)
library(spData)
library(leaflet)
library(tidyverse)
world_coffee = left_join(world, coffee_data)
pal = colorNumeric(palette = "RdYlBu", domain = c(0, 4000))

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

  setBookmarkExclude(names = c("lat", "lng", "year", "map_bounds", "north", "east", "south", "west", "legend", "range"))

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

  output$text = renderText(input$map_zoom)

}

shinyApp(ui, server, enableBookmarking = "url")
