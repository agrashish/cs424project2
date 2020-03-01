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


ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)