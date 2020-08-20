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
calendar_prices <- left_join(calendar_prices, all_listings[ , c("id", "neighbourhood")],
                             by= c("listing_id" ="id"))

price_by_type_neighbourhood <- all_listings %>%
  group_by(room_type, neighbourhood) %>%
  summarise(price = mean(price), n = n())

neighborhood_choice <- sort(unique(all_listings$neighbourhood))

price_quantiles <- price_by_type_neighbourhood$price %>%
  quantile(c(1/3, 2/3, 1)) %>%
  unname()

price_cat <- all_listings %>%
  mutate(price_category = ifelse(price <= price_quantiles[1], 'Low', 
                                 ifelse(price > price_quantiles[1] & price <= price_quantiles[2],
                                        'Medium', 'High')))
pal <- colorFactor(
  palette = "viridis",
  domain = price_cat$price_category)

attractions <- readRDS("All_Data/attractions.rds")

all_attractions <- attractions %>%
  filter(lat <= 33 & lon <= -110)

restaurants <- readRDS("All_Data/restaurants.rds")

all_restaurants <- restaurants %>%
  filter(lat <= 33 & lon <= -110)


#leafletOutput("map", width="100%", height="100%"),
#
## Shiny versions prior to 0.11 should use class = "modal" instead.
#absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
#              width = 330, height = "auto",

############## User interface ----
ui <- fluidPage(
  
  titlePanel("San Diego Airbnb"),
  
 #leafletOutput("mymap", width="100%", height="100%"),
 #
 #absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
 #              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
 #              width = 330, height = "auto",
 #              
 #              #h2("San Diego Airbn"),
 #              
 #              selectInput("Barrio", "Select a neighborhood", neighborhood_choice),
 #              #selectInput("size", "Size", vars, selected = "adultpop"),
 #          
 #              plotOutput("numero_listings", height = 250),
 #              plotOutput("precio_tiempo", height = 250))
 #)
#,
#  
#  
 sidebarLayout(
   
   sidebarPanel(
     
     fluidRow(
       selectInput("Barrio", "Select a neighborhood", neighborhood_choice, selected = "Pacific Beach")),
     
     fluidRow(
       plotOutput("numero_listings", height = 200)),
     
     fluidRow(
       plotOutput("precio_tiempo", height = 200)),
     
     fluidRow(
       checkboxGroupInput("markers", "Choose extras to show", 
                          choices = c("Restaurants", "Attractions"), 
                          selected = "Restaurants")
     )
     
     ),
   mainPanel(leafletOutput("mymap", height = 600))
   )
   )


server <- function(input, output, session) {
  
 # output$hourlyPlot <- renderPlot({
 #   employ %>%
 #     filter(full_time == input$full_time,
 #            wage >= input$wage[[1]],
 #            wage <= input$wage[[2]]) %>%
 #     ggplot(aes(wage)) +
 #     geom_histogram()
 # })
 # 
 #selectedData <- reactive({
 #  req(input$Barrio)
 #  
 #  price_cat %>% 
 #    dplyr::filter(neighborhood %in% input$Barrio)
 #})
 ## output$hourlyPlot <- renderPlot({
 #   plot(rnorm(input$wage[1]))
 # })
  
  #Filter y filtras por input$ y el nombre del input
  
  #output$
  #server <- function(input, output, session) {
    # Some random data:
    #dat <- data.frame(long = rnorm(40) * 2 + 13, lat = rnorm(40) + 48)
    
    # observe() looks for changes in input$markers and adds/removes
    # markers as necessary:

    
    # Render basic map with any element that will not change.
    # Note: you can change the starting zoom/positioning/et cetera
    # as appropriate:

  
  output$precio_tiempo <- renderPlot({
    calendar_prices %>%
      filter(neighbourhood %in% input$Barrio) %>%
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
  
 # ggplot(data=citibike,aes(x=start.hour)) + 
 #   geom_bar() + 
 #   labs(x = 'Time of Day',
 #        y = 'Number of Trips')+
 #   theme(axis.text.x  = element_text(size=8,angle=90))
  
  #rests<- reactive({
  #  all_restaurants
  #})
  #
  #attrs <- reactive({
  #  all_attractions
  #})
  #
  output$numero_listings <- renderPlot({
    
    all_listings %>%
      filter(neighbourhood %in% input$Barrio) %>%
      filter(!is.na(room_type)) %>%
      group_by(room_type) %>%
      summarize(n = n()) %>%
      
      ggplot(aes(x=room_type, y=n, fill = room_type)) +
      geom_bar(stat = 'identity') +
      ggtitle('Airbnb Listings by type') +
      labs(x = 'Room type',
           y = 'Number of listings') +
      #coord_polar("y", start = 0) +
      theme(axis.text.x = element_text(size=10), legend.position = "none")
  })
  



output$mymap <- renderLeaflet({
  #display()
  
    para_graf <- price_cat %>%
      filter(neighbourhood %in% input$Barrio)
    
  leaflet(barrio) %>%
    addTiles() %>%
    addPolygons(weight = 1, color = "#444444", fill = FALSE) %>%
    addCircles(lng = ~para_graf$longitude, lat = ~para_graf$latitude, label = ~as.character(para_graf$price), color = ~pal(para_graf$price_category)) %>%
    addLegend("bottomleft", pal = pal, values = ~para_graf$price_category, title = "Price Category") %>%
    setView(lng = mean(para_graf$longitude), lat = mean(para_graf$latitude), zoom = 12.85) %>%
    addMarkers(lng = ~all_restaurants$lon, lat = ~all_restaurants$lat, label = ~as.character(all_restaurants$restaurant_names))
    
})
    
}

# Run app ----
shinyApp(ui, server)