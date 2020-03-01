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
rawdata$Date <- as.character(rawdata$Date)
rawdata$Time <- as.character(rawdata$Time)
rawdata$RecordID <- as.character(rawdata$RecordID)
rawdata$Status <- as.character(rawdata$Status)
rawdata$Lat <- as.numeric(rawdata$Lat)
rawdata$Long <- as.numeric(rawdata$Long)

#get the year of the hurricane from the start string
rawdata$Year = lapply(rawdata$Hurricane, function(x){
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

#make hurricanes with only one zero have four instead(formatting reasons)
rawdata$Time[which(rawdata$Time == "0")]<- "0000"

#Convert the date column to correct format
rawdata$Date <- as.Date(rawdata$Date, format = "%Y%m%d")

#get the Minute the cyclone occurred
rawdata$Minute <- lapply(rawdata$Time, function(x){
  as.integer(substr(x,nchar(x)-1,nchar(x)))
})

#get the Hour the cyclone occurred
rawdata$Hour <- lapply(rawdata$Time, function(x){
  as.integer(substr(x,nchar(x)-3,nchar(x)-2))
})

#Convert the date column and times to datetime format
rawdata$DateandTimes <- as.POSIXct(paste(rawdata$Date, rawdata$Hour, rawdata$Minute), format = "%Y-%m-%d %H%M", tz="GMT")

#clear up data a bit
rawdata$DateandTimes <- as.character(rawdata$DateandTimes)

#get top 10 list
temp <- rawdata[rev(order(rawdata$MaxWind)),]
temp <- head(temp[!duplicated(temp["Name"]),],10)
rownames(temp) <- c()
top10 <- rawdata[which(rawdata$Name %in% temp$Name),]
top10 <- top10[rev(order(top10$MaxWind)),]

#names of hurricanes that have year >= 2005
names <- unique(rawdata[rawdata$Year >= 2005,]$Name)
names <- sapply(names, function(x){x})

#years >= 2005 in dataset
years <- unique(rawdata$Year)
years <- years[years >= 2005]

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 6, title = "Atlantic Hurricane Map", leafletOutput("atlanticMap")),
      box(width = 6, 
          selectInput(
            "pickFilter", "Select How to Filter Hurricanes (since 2005): ", 
            choices = c("Current Season", "All", "Year", "Individual", "Top 10")
          ),
          uiOutput("picker")
      ),
    ),
    fluidRow(
      box(width = 6, title = "Hurricane List", DT::dataTableOutput("orderHurricane")),
      box(width = 6, selectInput("orderFilter", "Select how to Order the Hurricane List: ", choices = c("Chronologically", "Alphabetically", "Max Wind Speed", "Minimum Pressure")))
    )
  )
)

server <- function(input, output) {
  output$picker <- renderUI({
    if(input$pickFilter == "Year") {
      selectInput("userFilter", "Select Year", choices = years)
    }
    else if(input$pickFilter == "Individual") {
      selectInput("userFilter", "Select Hurricane", choices = names)
    }
    else {
      
    }
  })
  
  rawdataFiltered <- reactive({
    if(input$pickFilter == "Current Season") {
      rawdataFiltered <- rawdata[rawdata$Year == 2018,]
    }
    else if(input$pickFilter == "All") {
      rawdataFiltered <- rawdata[rawdata$Year >= 2005,]
    }
    else if(input$pickFilter == "Year") {
      rawdataFiltered <- rawdata[rawdata$Year == input$userFilter,]
    }
    else if(input$pickFilter == "Individual") {
      rawdataFiltered <- rawdata[rawdata$Name == input$userFilter,]
    }
    else if(input$pickFilter == "Top 10") {
      rawdataFiltered <- top10
    }
    rawdataFiltered
  })  
  
  output$atlanticMap <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- addMarkers(map = map, data = rawdataFiltered(), lat = ~Lat, lng = ~Long, clusterOptions = markerClusterOptions())
    map <- addLayersControl(map = map, overlayGroups = rawdataFiltered()$Name)
    map
  })
  
  orderdataFiltered <- reactive({
    if(input$orderFilter == "Chronologically"){
      chronological <- as.data.frame(lapply(rawdata, unlist))
      attach(chronological)
      chronological <- chronological[order(DateandTimes),]
      detach(chronological)
      chronological <- subset(chronological, select = c(Hurricane, Name, DateandTimes))
      orderdataFiltered <- chronological
    }
    else if(input$orderFilter == "Alphabetically"){
      alphabetic <- as.data.frame(lapply(rawdata, unlist))
      attach(alphabetic)
      alphabetic <- alphabetic[order(Name),]
      detach(alphabetic)
      alphabetic <- subset(alphabetic, select = c(Hurricane, Name, DateandTimes))
      alphabetic <- alphabetic[!duplicated(alphabetic$Hurricane),]
      orderdataFiltered <- alphabetic
    }
    else if(input$orderFilter == "Max Wind Speed"){
      mWindSpeed <- as.data.frame(lapply(rawdata, unlist))
      attach(mWindSpeed)
      mWindSpeed <- mWindSpeed[order(-MaxWind),]
      detach(mWindSpeed)
      mWindSpeed <- subset(mWindSpeed, select = c(Hurricane, Name, MaxWind))
      mWindSpeed <- mWindSpeed[!duplicated(mWindSpeed$Hurricane),]
      orderdataFiltered <- mWindSpeed
    }
    else if(input$orderFilter == "Minimum Pressure"){
      mPressure <- as.data.frame(lapply(rawdata, unlist))
      attach(mPressure)
      mPressure <- mPressure[order(MinPress),]
      detach(mPressure)
      mPressure <- subset(mPressure, select = c(Hurricane, Name, MinPress))
      mPressure <- mPressure[!duplicated(mPressure$Hurricane),]
      orderdataFiltered <- mPressure
    }
    orderdataFiltered
  })
  
  output$orderHurricane <- DT::renderDataTable({
    as.data.frame(orderdataFiltered())
  })
}

shinyApp(ui = ui, server = server)