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
rawdata$Lat <- as.character(rawdata$Lat)
rawdata$Long <- as.character(rawdata$Long)

#replace lat strings with 'N' or 'S' to proper numeric
rawdata$Lat <- sapply(rawdata$Lat, function(x) {
  if(substr(x,nchar(x),nchar(x))[1] == 'N') {
    as.numeric(substr(x,0,nchar(x)-1))
  }
  else {
    - as.numeric(substr(x,0,nchar(x)-1))
  }
})

#replace long strings with 'E' or 'W' to proper numeric
rawdata$Long <- sapply(rawdata$Long, function(x) {
  if(substr(x,nchar(x),nchar(x))[1] == 'E') {
    as.numeric(substr(x,0,nchar(x)-1))
  }
  else {
    - as.numeric(substr(x,0,nchar(x)-1))
  }
})


#get the year of the hurricane from the start string
rawdata$Year = lapply(rawdata$Hurricane, function(x){
  as.integer(substr(x,nchar(x)-3,nchar(x)))
})

#trim the whitespaces from the name
rawdata$Name <- lapply(rawdata$Name, trimws)

#make unnamed hurricanes display their year and number
rawdata$Name <- 
  paste(rawdata$Name,
        " (", 
        rawdata$Hurricane, 
        ")"
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
temp <- head(temp[!duplicated(temp["Hurricane"]),],10)
rownames(temp) <- c()
top10 <- rawdata[which(rawdata$Name %in% temp$Name),]
top10 <- top10[rev(order(top10$MaxWind)),]

#names of hurricanes that have year >= 2005
names <- unique(rawdata[rawdata$Year >= 2005,]$Name)
names <- sapply(names, function(x){x})

#years >= 2005 in dataset
years <- unique(rawdata$Year)
years <- years[years >= 2005]

rawdata$factors <- as.factor(rawdata$Hurricane)

########### SECOND FILE ##############
#read in second file
rawdata2ndFile <- read.csv(file = "hurdat2Pacific-formatted.txt")

#cleanup
rawdata2ndFile$Hurricane <- as.character(rawdata2ndFile$Hurricane)
rawdata2ndFile$Name <- as.character(rawdata2ndFile$Name)
rawdata2ndFile$RecordID <- as.character(rawdata2ndFile$RecordID)
rawdata2ndFile$Status <- as.character(rawdata2ndFile$Status)
rawdata2ndFile$Lat <- as.numeric(rawdata2ndFile$Lat)
rawdata2ndFile$Long <- as.numeric(rawdata2ndFile$Long)

#get the year of the hurricane from the start string
rawdata2ndFile$Year = lapply(rawdata2ndFile$Hurricane, function(x){
  as.integer(substr(x,nchar(x)-3,nchar(x)))
})

#trim the whitespaces from the name
rawdata2ndFile$Name <- lapply(rawdata2ndFile$Name, trimws)

#get the cyclone number of the hurricane from the start string
rawdata2ndFile$CycNum <- lapply(rawdata2ndFile$Hurricane, function(x){
  as.integer(substr(x,nchar(x)-5,nchar(x)-4))
})

#make unnamed hurricanes display their year and number
rawdata2ndFile$Name[which(rawdata2ndFile$Name == "UNNAMED")] <- 
  paste("UNNAMED (", 
        rawdata2ndFile$Hurricane[which(rawdata2ndFile$Name == "UNNAMED")], 
        ")"
  )

#get the day the cyclone occurred
rawdata2ndFile$Day <- lapply(rawdata2ndFile$Date, function(x){
  as.integer(substr(x,nchar(x)-1,nchar(x)))
})

#get the month the cyclone occurred
rawdata2ndFile$Month <- lapply(rawdata2ndFile$Date, function(x){
  as.integer(substr(x,nchar(x)-3,nchar(x)-2))
})

#get the Minute the cyclone occurred
rawdata2ndFile$Minute <- lapply(rawdata2ndFile$Time, function(x){
  as.integer(substr(x,nchar(x)-1,nchar(x)))
})

#get the Hour the cyclone occurred
rawdata2ndFile$Hour <- lapply(rawdata2ndFile$Time, function(x){
  as.integer(substr(x,nchar(x)-3,nchar(x)-2))
})

#Convert the date column and times to datetime format
rawdata2ndFile$DateandTimes <- as.POSIXct(paste(rawdata2ndFile$Date, rawdata2ndFile$Hour, rawdata2ndFile$Minute), format = "%Y-%m-%d %H%M", tz="GMT")

#clear up data a bit
rawdata2ndFile$DateandTimes <- as.character(rawdata2ndFile$DateandTimes)

rawdata2ndFile$factors <- as.factor(rawdata2ndFile$Hurricane)

######################################

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      #Atlantic Map
      box(width = 6, title = "Atlantic Hurricane Map", selectInput("pickFilter", "Select How to Filter Hurricanes (since 2005): ", choices = c("Current Season", "All", "Year", "Individual", "Top 10")), uiOutput("picker"),leafletOutput("atlanticMap"),),
      #Pacific Map
      box(width = 6, title = "Pacific Hurricane Map", selectInput("pickFilter2", "Select How to Filter Hurricanes (since 2005): ", choices = c("Current Season", "All", "Year", "Individual", "Top 10")), uiOutput("picker2"),leafletOutput("pacificMap"),)
    ),
    fluidRow(
      #Atlantic
      box(width = 6, title = "Atlantic Hurricane List", selectInput("orderFilter", "Select how to Order the Hurricane List: ", choices = c("Chronologically", "Alphabetically", "Max Wind Speed", "Minimum Pressure")),DT::dataTableOutput("orderHurricane") ),
      #Pacific
      box(width = 6, title = "Pacific Hurricane List", selectInput("orderFilter2", "Select how to Order the Hurricane List: ", choices = c("Chronologically", "Alphabetically", "Max Wind Speed", "Minimum Pressure")),DT::dataTableOutput("orderHurricane2") )
    ),
    fluidRow(
      box(width = 12,
          mainPanel(width = 6, 
                    tabsetPanel(
                      tabPanel("(Alt)Hurricane by Year(2005-2018)",      
                               ##Atlantic year chart
                               box( width = 12,title = "Hurricane by Year", status = "primary", solidHeader = TRUE, plotOutput("hurricanesYearlyHistogram", height = 360)   
                               )
                      ),
                      tabPanel("(Alt)Hurricane by Status(2005-2018)",
                               ##Atlantic Status chart
                               box( width = 12,title = "Hurricane by Status", status = "primary", solidHeader = TRUE, plotOutput("hurricanesByStatusHistogram", height = 360)   
                               )
                      )
                    )
          ),
          mainPanel(width = 6, 
                    tabsetPanel(
                      tabPanel("(Pt)Hurricane by Year(2005-2018)",      
                               ##APacific year chart
                               box( width = 12,title = "Hurricane by Year", status = "primary", solidHeader = TRUE, plotOutput("hurricanesYearlyHistogramPacific", height = 360)   
                               )
                      ),
                      tabPanel("(Pt)Hurricane by Status(2005-2018)",
                               ##Pacific Status chart
                               box( width = 12,title = "Hurricane by Status", status = "primary", solidHeader = TRUE, plotOutput("hurricanesByStatusHistogramPacific", height = 360)   
                               )
                      )
                    )
          )
      )
    )
  )
)

server <- function(input, output) {
  
  #If User selected a certain option do something to Atlantic map
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
  
  #If user selected a certain option do something to Pacific Map
  output$picker2 <- renderUI({
    if(input$pickFilter2 == "Year") {
      selectInput("userFilter2", "Select Year", choices = years)
    }
    else if(input$pickFilter2 == "Individual") {
      selectInput("userFilter2", "Select Hurricane", choices = names)
    }
    else {
      
    }
  })
  
  #For the antlantic map
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
  
  #For the Pacific Map
  rawdataFiltered2 <- reactive({
    if(input$pickFilter2 == "Current Season") {
      rawdataFiltered2 <- rawdata2ndFile[rawdata2ndFile$Year == 2018,]
    }
    else if(input$pickFilter2 == "All") {
      rawdataFiltered2 <- rawdata2ndFile[rawdata2ndFile$Year >= 2005,]
    }
    else if(input$pickFilter2 == "Year") {
      rawdataFiltered2 <- rawdata2ndFile[rawdata2ndFile$Year == input$userFilter2,]
    }
    else if(input$pickFilter2 == "Individual") {
      rawdataFiltered2 <- rawdata2ndFile[rawdata2ndFile$Name == input$userFilter2,]
    }
    else if(input$pickFilter2 == "Top 10") {
      rawdataFiltered2 <- top10
    }
    rawdataFiltered2
  })  
  
  #Map for the atlantic data
  output$atlanticMap <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    pal <- colorFactor(topo.colors(length(unique(rawdata$Hurricane))), rawdata$factors)
    map <- addCircleMarkers(map = map, data = rawdataFiltered(), group = ~Name, lat = ~Lat, lng = ~Long, color = ~pal(factors), radius = ~2*log(MaxWind))
    for(factor in levels(rawdataFiltered()$factors)) {
      map <- addPolylines(map, data=rawdataFiltered()[rawdataFiltered()$factors==factor,], lat=~Lat, lng=~Long, color = ~pal(factors), weight = ~2*log(MaxWind), group = ~Name)
    }
    map <- addLayersControl(map = map, overlayGroups = rawdataFiltered()$Name)
    map
  })
  
  #Map for the pacific data
  output$pacificMap <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    pal <- colorFactor(topo.colors(length(unique(rawdata2ndFile$Hurricane))), rawdata2ndFile$factors)
    map <- addCircleMarkers(map = map, data = rawdataFiltered2(), group = ~Name, lat = ~Lat, lng = ~Long, color = ~pal(factors), radius = ~2*log(MaxWind))
    for(factor in levels(rawdataFiltered2()$factors)) {
      map <- addPolylines(map, data=rawdataFiltered2()[rawdataFiltered2()$factors==factor,], lat=~Lat, lng=~Long, color = ~pal(factors), weight = ~2*log(MaxWind), group = ~Name)
    }
    map <- addLayersControl(map = map, overlayGroups = rawdataFiltered2()$Name)
    map
  })
  
#for the first file
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
  
#for the second file
  orderdataFiltered2 <- reactive({
    if(input$orderFilter2 == "Chronologically"){
      chronological <- as.data.frame(lapply(rawdata2ndFile, unlist))
      attach(chronological)
      chronological <- chronological[order(DateandTimes),]
      detach(chronological)
      chronological <- subset(chronological, select = c(Hurricane, Name, DateandTimes))
      orderdataFiltered2 <- chronological
    }
    else if(input$orderFilter2 == "Alphabetically"){
      alphabetic <- as.data.frame(lapply(rawdata2ndFile, unlist))
      attach(alphabetic)
      alphabetic <- alphabetic[order(Name),]
      detach(alphabetic)
      alphabetic <- subset(alphabetic, select = c(Hurricane, Name, DateandTimes))
      alphabetic <- alphabetic[!duplicated(alphabetic$Hurricane),]
      orderdataFiltered2 <- alphabetic
    }
    else if(input$orderFilter2 == "Max Wind Speed"){
      mWindSpeed <- as.data.frame(lapply(rawdata2ndFile, unlist))
      attach(mWindSpeed)
      mWindSpeed <- mWindSpeed[order(-MaxWind),]
      detach(mWindSpeed)
      mWindSpeed <- subset(mWindSpeed, select = c(Hurricane, Name, MaxWind))
      mWindSpeed <- mWindSpeed[!duplicated(mWindSpeed$Hurricane),]
      orderdataFiltered2 <- mWindSpeed
    }
    else if(input$orderFilter2 == "Minimum Pressure"){
      mPressure <- as.data.frame(lapply(rawdata2ndFile, unlist))
      attach(mPressure)
      mPressure <- mPressure[order(MinPress),]
      detach(mPressure)
      mPressure <- subset(mPressure, select = c(Hurricane, Name, MinPress))
      mPressure <- mPressure[!duplicated(mPressure$Hurricane),]
      orderdataFiltered2 <- mPressure
    }
    orderdataFiltered2
  })
  
  
  output$orderHurricane <- DT::renderDataTable({
    as.data.frame(orderdataFiltered())
  })
  
  output$orderHurricane2 <- DT::renderDataTable({
    as.data.frame(orderdataFiltered2())
  })
  
  output$hurricanesYearlyHistogram <- renderPlot({
    ## graph for total hurricanes in a year##
    ##get rid of duplicates and greater than year:2005
    temp <- rawdata[rev(order(rawdata$MaxWind)),]
    temp <- temp[!duplicated(temp["Hurricane"]),]
    temp <- temp[temp$Year >= 2005,]
    year <- as.integer(temp$Year)
    years<- factor(year)
    p <- ggplot(temp) + aes(x = years) + geom_bar(color = "black", fill="blue") + theme_dark()
    p
  })
  
  output$hurricanesByStatusHistogram <- renderPlot({
    ## graph for total hurricanes in a specific Status##
    ##get rid of duplicates and greater than year:2005##
    temp <- rawdata[rev(order(rawdata$MaxWind)),]
    temp <- temp[!duplicated(temp["Hurricane"]),]
    temp <- temp[temp$Year >= 2005,]
    q <- ggplot(temp) + aes(x = Status) + geom_bar(color = "black", fill="blue")  + theme_dark()
    q
  })
  
  output$hurricanesYearlyHistogramPacific <- renderPlot({
    ## graph for total hurricanes in a year##
    ##get rid of duplicates and greater than year:2005
    temp2 <- rawdata2ndFile[rev(order(rawdata2ndFile$MaxWind)),]
    temp2 <- temp2[!duplicated(temp2["Hurricane"]),]
    temp2 <- temp2[temp2$Year >= 2005,]
    year2 <- as.integer(temp2$Year)
    years2<- factor(year2)
    p <- ggplot(temp2) + aes(x = years2) + geom_bar(color = "black", fill="maroon") + theme_dark()
    p
  })
  
  output$hurricanesByStatusHistogramPacific <- renderPlot({
    ## graph for total hurricanes in a specific Status##
    temp2 <- rawdata2ndFile[rev(order(rawdata2ndFile$MaxWind)),]
    temp2 <- temp2[!duplicated(temp2["Hurricane"]),]
    temp2 <- temp2[temp2$Year >= 2005,]
    q <- ggplot(temp2) + aes(x = Status) + geom_bar(color = "black", fill="maroon")  + theme_dark()
    q
  })
  
}

shinyApp(ui = ui, server = server)