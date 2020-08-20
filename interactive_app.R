# Load packages ----
library(shiny)
library(tidyverse)
library(geojsonR)
library(leaflet)
library(lubridate)
library(forcats)
library(leaflet.extras)

# Load data ----
#counties <- readRDS("data/counties.rds")
all_listings <- read_rds("All_Data/all_listings.rds")
barrio<- rgdal::readOGR("All_Data/2018-Sep/neighbourhoods.geojson")
calendar_prices <- read_rds("All_Data/calendar_prices.rds")

calendar_prices$price = as.numeric(gsub("\\$", "", calendar_prices$price))

# Source helper functions -----
#source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("San Diego Airbnb"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
      helpText("Explore San Diego neighborhoods.")),
      
      fluidRow(
      
      selectInput("var", 
                  label = "Choose a neighbprhood to display",
                  choices = c("N1", "N2",
                              "N3", "N4"),
                  selected = "N1")),
      fluidRow(
      plotOutput("torta")),
      
      fluidRow(
        plotOutput("precio_tiempo")
      )
    ),
    
    mainPanel(leafletOutput("mymap"))
  )
)


# Server logic ----
server <- function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet(barrio) %>%
      addTiles() %>%
      addPolygons(weight = 1, color = "#444444", fill = FALSE)
  })
  
  output$precio_tiempo <- renderPlot({
    calendar_prices %>%
      mutate(month_year = format(as.Date(date), "%Y-%m")) %>%
      group_by(month_year) %>%
      summarise(avg_price = mean(price,na.rm = TRUE)) %>%
      ggplot(aes(x=month_year,y=avg_price, group = 1)) +
      geom_point() +
      geom_line(linetype='dotted') +
      labs(y = 'Price',
           x = 'Month',
           title = 'Price evolution on time')
    
  })
  
  
  output$torta <- renderPlot({
    all_listings %>%
      filter(!is.na(room_type)) %>%
      group_by(room_type) %>%
      summarize(n = n()) %>%
      
      ggplot(aes(x="", y=n,fill=room_type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0)
  })
  
}
  

# Run app ----
shinyApp(ui, server)
