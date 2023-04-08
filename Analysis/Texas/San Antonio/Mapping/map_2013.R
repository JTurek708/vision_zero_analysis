### Shiny Map of 2013 Crash Data
library(leaflet)
library(shiny)
library(rsconnect)

ped_cycle <- read.csv("ped_cycle.csv", header = TRUE, stringsAsFactors = FALSE)
# Remove null values
ped_cycle <- ped_cycle[complete.cases(ped_cycle), ]
# Write to csv
write.csv(ped_cycle, "ped_cycle.csv", row.names = FALSE)

ui <- fluidPage(
  titlePanel("San Antonio Pedestrian & Cyclist Crash Map - 2013 (v1)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Person Type:",
                  choices = c("All", unique(ped_cycle$Person_Type))),
      selectInput("severity", "Injury Severity:",
                  choices = c("All", unique(ped_cycle$Person_Injury_Severity)))
    ),
    mainPanel(
      leafletOutput("map", height = "800px", width = "90%")
    )
  )
)

server <- function(input, output, session) {
  library(tidyverse)
  library(ggrepel)
  library(leaflet)
  library(leaflet.extras)
  library(shiny)
  ped_cycle <- read.csv("ped_cycle.csv", header = TRUE, stringsAsFactors = FALSE)
  # Filter data based on user input
  ped_cycle_filter <- reactive({
    ped_cycle %>%
      filter(Person_Type %in% input$type | 
               input$type == "All",
             Person_Injury_Severity %in% input$severity | 
               input$severity == "All")
  })
  
  # Create map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.4936, lat = 29.4241, zoom = 11) %>%
      addCircleMarkers(data = ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),], 
                       lng = ~Longitude, lat = ~Latitude,
                       color = 'black',
                       fillColor = ~ifelse(Person_Type == "3 - PEDALCYCLIST", "blue", "green"),
                       radius = 10, weight = .5, opacity = 1, fillOpacity = 1,
                       group = ~Person_Type,
                       popup = paste("<strong>Person Type:</strong> ", ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Type, "<br>",
                                     "<strong>Crash Date: ", ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Crash_Date, "<br>",
                                     "<strong>Street Number: ", ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Street_Number, "<br>",
                                     "<strong>Street Name: ", ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Street_Name, "<br>",
                                     "<strong>Person Injury Severity: ", ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Injury_Severity, "<br>",
                                     "<strong>Fatalities: ", ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Death_Count, "<br>"
                       )) 
    # %>%
    #   addLegend(position = "bottomright",
    #             colors = c("blue", "green"),
    #             labels = c("Cyclist", "Pedestrian"),
    #             title = "Person Type")
    
    # Add filter control
    # my_map <- my_map %>%
    #   addLayersControl(
    #     overlayGroups = c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),
    #     options = layersControlOptions(collapsed = FALSE, position = "topright")
    #   ) 
    #my_map
  })
}

shinyApp(ui, server)



