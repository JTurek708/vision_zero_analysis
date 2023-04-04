### San Antonio 2013 EDA & Mapping Test Run
####### load packages ######
library(lubridate)
install.packages("mapview")
library(ggmap)
library(leaflet)
install.packages("leaflet.extras")
library(leaflet.extras)
library(leaflet.providers)
library(tidyverse)
library(osmdata)
library(crosstalk)
library(tigris)
library(sf)
library(htmlwidgets)
#########################

# Adjust Crash Date column and add a Month column
san_antonio_updated_crash_2013$`Crash Date` <- mdy(san_antonio_updated_crash_2013$`Crash Date`)
san_antonio_updated_crash_2013$Crash_Month <- month(san_antonio_updated_crash_2013$`Crash Date`)
san_antonio_updated_crash_2013$Crash_Month <- month.name[san_antonio_updated_crash_2013$Crash_Month]

# Prelim eda
san_antonio_updated_crash_2013 %>%
  group_by(`Person Type`) %>%
  summarize(n = n())

san_antonio_updated_crash_2013 %>%
  group_by(`Crash Severity`) %>%
  summarize(n = n())

san_antonio_updated_crash_2013 %>%
  group_by(`Person Injury Severity`) %>%
  summarise(n = n())

# Replace column header spaces
colnames(san_antonio_updated_crash_2013) <- gsub(" ", "_", colnames(san_antonio_updated_crash_2013))

# Check Lat/Long class and change if necessary
class(san_antonio_updated_crash_2013$Latitude)
class(san_antonio_updated_crash_2013$Longitude)
san_antonio_updated_crash_2013$Latitude <- as.numeric(san_antonio_updated_crash_2013$Latitude)
san_antonio_updated_crash_2013$Longitude <- as.numeric(san_antonio_updated_crash_2013$Longitude)
san_antonio_updated_crash_2013$Street_Number <- as.numeric(san_antonio_updated_crash_2013$Street_Number)

### Basic Mapping ###
# Filter for new df
ped_cycle <- san_antonio_updated_crash_2013 %>%
  filter(Person_Type == "3 - PEDALCYCLIST" |
           Person_Type == "4 - PEDESTRIAN")
View(ped_cycle)

# Get zip codes for SATX
#zipcodes <- opq("San Antonio, TX") %>%
#  add_osm_feature(key = "postal code") %>%
#  osmdata_sf()
#View(zipcodes)
# Add zip code polygons to map - Later 
#my_map <- my_map %>%
#  addPolygons(data = zipcodes$osm_polygons,
#              fillColor = "#ffffcc", color = "#666666", weight = 1,
# group = "Zip Code",
#              fillOpacity = 1)

# Create map
my_map <- leaflet(ped_cycle) %>%
  addTiles() %>%
  setView(lng = -98.4936, lat = 29.4241, zoom = 11)

# Add bicyclist icon
bicyclistIcon <- makeIcon(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "https://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

# Add pedestrian icon
pedestrianIcon <- makeIcon(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-red.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "https://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

# create Person Injury Severity color palette
color_palette <- colorFactor(palette = "Set1", domain = ped_cycle$Person_Injury_Severity)

# Add search bar
my_map <- addSearchOSM(my_map)

# Add markers to the map
my_map <- my_map %>%
  addCircleMarkers(data = ped_cycle[ped_cycle$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"), ], 
                   lng = ~Longitude, lat = ~Latitude,
                   color = 'black',
                   fillColor = ~ifelse(Person_Type == "3 - PEDALCYCLIST", "blue", "green"),
                   radius = 4, weight = .5, opacity = 1, fillOpacity = 1,
                   group = ~Person_Type,
                   popup = paste("<strong>Person Type:</strong> ", ped_cycle[ped_cycle$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"), ]$Person_Type, "<br>",
                                 "<strong>Crash Date: ", ped_cycle[ped_cycle$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"), ]$Crash_Date, "<br>",
                                 "<strong>Street Number: ", ped_cycle[ped_cycle$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"), ]$Street_Number, "<br>",
                                 "<strong>Street Name: ", ped_cycle[ped_cycle$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"), ]$Street_Name, "<br>",
                                 "<strong>Person Injury Severity: ", ped_cycle[ped_cycle$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"), ]$Person_Injury_Severity, "<br>",
                                 "<strong>Fatalities: ", ped_cycle[ped_cycle$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Death_Count, "<br>"
                                 ))
# Add filter control
my_map <- my_map %>%
  addLayersControl(
    overlayGroups = c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"), #"Zip Code"#),
    options = layersControlOptions(collapsed = FALSE, position = "topright")
  )
    
my_map
