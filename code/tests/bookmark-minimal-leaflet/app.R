# from https://github.com/Robinlovelace/geocompr/blob/master/coffeeApp/app.R
library(sf)
library(shiny)
library(spData)
library(leaflet)
library(tidyverse)
library(tmap)
tmap_mode("view")

rnet = readRDS("school_data_desire_lines_chepstow.Rds")
b_pct = c(0, 1, 2, 5, 10, 20, 50, 100, 500)

ui = function(request) {
  fluidPage(
    sidebarPanel(
      textOutput(outputId = "text"),
      selectInput("year", "Year (Example)", c(2016, 2017)),
    ),
    mainPanel(
      tmap::tmapOutput("map")
    )
  )
}
server = function(input, output, session) {

  m = tm_shape(rnet) +
    tm_lines(col = "n", breaks = b_pct,
             # lwd = "n",
             scale = 4,
             palette = "Blues") +
    tm_scale_bar()

  output$map = tmap::renderTmap(m)

}

shinyApp(ui, server, enableBookmarking = "url")
