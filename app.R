#Project 2
#Aashish Agrawal - aagraw10
#Ivan Madrid - 
#Richard Miramontes - 
#CS 424

#load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)

#read in data from csv
rawdata <- read.csv(file = "hurdat2-formatted.txt")

#clear up data a little bit
rawdata$Hurricane <- as.character(rawdata$Hurricane)
rawdata$Name <- as.character(rawdata$Name)
rawdata$RecordID <- as.character(rawdata$RecordID)
rawdata$Status <- as.character(rawdata$Status)
rawdata$Lat <- as.numeric(rawdata$Lat)
rawdata$Long <- as.numeric(rawdata$Long)

#get the year of the hurricane from the start string
rawdata$Year <- lapply(rawdata$Hurricane, function(x){
  as.integer(substr(x,nchar(x)-3,nchar(x)))
})

#trim the whitespaces from the name
rawdata$Name <- lapply(rawdata$Name, trimws)

#get the cyclone number of the hurricane from the start string
rawdata$CycNum <- lapply(rawdata$Hurricane, function(x){
  as.integer(substr(x,nchar(x)-5,nchar(x)-4))
})

#make unnamed hurricanes display their year and number
rawdata$Name[which(rawdata$Name == "UNNAMED")] <- 
  paste("Unnamed-", 
        rawdata$Year[which(rawdata$Name == "UNNAMED")], 
        "-", rawdata$CycNum[which(rawdata$Name == "UNNAMED")]
  )


ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 6, title = "Atlantic Hurricane Map", leafletOutput("atlanticMap")),
      selectInput("pickFilter", "Select How to Filter Hurricanes: ", choices = c("All", "By Year")),
    )
  )
)

server <- function(input, output) {
  output$atlanticMap <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- addMarkers(map = map, data = rawdata, lat = ~Lat, lng = ~Long, clusterOptions = markerClusterOptions())
    #map <- addLayersControl(map = map, overlayGroups = rawdata$Hurricane)
    map
  })
}

shinyApp(ui = ui, server = server)