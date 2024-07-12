### Shiny Map of 2013 Crash Data
library(leaflet)
install.packages("shiny")
library(shiny)
# install.packages("shinydashboard")
library(shinydashboard)
# install.packages("DT")
library(DT)
library(rsconnect)
library(sp)
library(sf)
library(RColorBrewer)
# install.packages("udunits2")
library(udunits2)
library(tidyverse)
ped_cycle_df <- read.csv("ped_cycle_df.csv", header = TRUE, stringsAsFactors = FALSE)

# Redistricted Council boundaries shapefile
shapefile <- sf::st_read(dsn = "RedistrictedCouncilDistricts2022.shp") %>%
  sf::st_transform(crs = 4326)
colorCount <- length(unique(shapefile$District))
colorPalette <- brewer.pal(colorCount, "Paired")
shapefile$ColorFactor <- factor(shapefile$District,
                                levels = unique(shapefile$District))
shapefile$color <- colorPalette[shapefile$ColorFactor]
# Bike Facilities shapefile
bike_shapefile <- sf::st_read(dsn = "Bike_Facilities.shp") %>%
  st_transform(crs = 4326)

#Shiny Code
ui <- dashboardPage(
  dashboardHeader(title = "San Antonio Vision Zero Crash Map",
                  titleWidth = "600px"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Crash Maps", icon = icon("map"),
               menuSubItem("Crash Map & Council Districts", tabName = "map_council"),
               menuSubItem("Cyclist Crashes & Bike Routes", tabName = "map_bike")),
      tags$li(tags$hr(style = "border-top: 1px solid #ffffff;")),
      menuItem("Data Description", tabName = "data_desc", icon = icon("database")),
      menuItem("Technical Notes", tabName = "tech_notes", icon = icon("cogs")),
      menuItem("About Me", tabName = "about_me", icon = icon("user")),
      tags$li(tags$hr(style = "border-top: 1px solid #ffffff;")),
      tags$li(class="treeview",
              tags$a(href = "https://docs.google.com/forms/d/e/1FAIpQLScPK3Yo3PQ6_a7uVNUy3Cyhvql7HBKKrDZcedz2bnq3H9Ex2w/viewform?usp=sf_link",
                     target="_blank",
                     tags$i(class = "fa fa-comments"), " Provide feedback here!"),
              style = "padding: 5px; margin-left: 3px; margin-right: 3px;"),
      tags$li(class="treeview",
              tags$a(href = "https://www.buymeacoffee.com/jturek708",
                     tags$i(class = "fa fa-pizza-slice"), "Buy Me a Pizza!"),
              style = "padding: 5px; margin-left: 3px; margin-right: 3px;")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom CSS class for map height */
        .map-container {
          height: 80vh; /* Adjust this value as needed */
        }
      ")),
      tags$head(tags$script(src = "https://plausible.io/js/script.js", defer = "defer", `data-domain` = "aj-vision-zero.uc.r.appspot.com"))),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(width = 7,
                       h2("San Antonio Vision Zero Crash Map Project"),
                       p("This map shows pedestrian and cyclist-involved crashes in San Antonio from 2013 to the present day.
                    San Antonio adopted its ", a("Vision Zero Plan", href="https://www.sa.gov/Directory/Departments/Transportation/Initiatives/Vision-Zero/Maps",
                                                 target="_blank")," in 2016. However, the plan contains no benchmarks for holding the City 
                    accountable to its Vision Zero Plan. The City is beginning to review and update its original plan, which is a step 
                    in the right direction. It is my hope that this dashboard can help both the city and residents have an informed discussion about 
                    essential projects and safety implementations to make San Antonio safer for those who decide or are forced to use alternate means of 
                    transportation other than a personal vehicle.", 
                         style = "padding: 15px;"),
                       p("The dashboard is set up to help understand where pedestrians and bicyclists are involved in crashes with vehicles. The idea for this project 
                     came about after I read several articles detailing European countries' successes at reducing pedestrian traffic deaths to almost 0. I knew that 
                     several cities in the U.S. have implemented Vision Zero plans but did not know how effective they have been at reducing roadway deaths. 
                     I began by collecting available data from every city that is listed on the Vision Zero Network's site as having a Vision Zero Plan. The data 
                     for those cities are available on my ", a("GitHub page", href="https://github.com/JTurek708/vision_zero_analysis", target="_blank"),".",
                         style="padding: 15px;"),
                       p("Using the data for San Antonio, I wanted to create a new kind of dashboard that showed all pedestrian and bicyclist crash incidents. The data I 
                    collected starts on January 1, 2013 and continues to the present day. I refresh the dashboard monthly with new data. The crash map available 
                    on San Antonio's Transportation Department's website is valuable, but I believe it does not show the full picture of how dangerous San Antonio's 
                    roads and sidewalks are for residents. The City's dashboard shows all crashes where there was a fatality or a severe injury. However, the data shown 
                    on the map only shows a limited range of years and is not filterable - users don't have the option to view only pedestrian crashes, or crashes from 
                    a certain year. This is where my dashboard fills that gap.",
                         style="padding: 15px;"),
                       p("The first map
                    shows San Antonio's City Council Districts overlayed across the crashes. You can also click on each individual 
                    point on the map to find out more information about the type of crash, the date of the crash, and any injuries. 
                    The second map shows only bicyclist crashes in San Antonio, along with the City's bike routes. While it may look like 
                    the City has an extensive bike route network, there's little to no connectivity between routes and many areas lack 
                    basic protection that would make cyclists safer.", 
                         style="padding: 15px;"),
                       p("All data was collected from the Texas Department of Transportation's Crash Query Information System.
                    The data presented here is likely an undercount of actual crashes involving pedestrians and cyclists.",
                         style="padding: 15px;"),
                       p("Check out my site for more information about Vision Zero and this project: ",
                         tags$a("https://jackturek708.com/category/vision-zero/",
                                href = "https://jackturek708.com/category/vision-zero/"),
                         style = "padding: 15px;")),
                column(width = 5,
                       box(
                         title = "Latest Updates!",
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         HTML("<b>December 8:</b> Refresh of dashboard site. Added feedback form to menu sidebar. Added button for help with server costs (Pizza). Added November 2023 crash data.<br>",
                              "<b>January 4:</b> December 2023 data updated and added to the dashboard.<br>",
                              "<b>February 4:</b> January 2024 data updated and added to the dashboard.<br>",
                              "<b>February 5:</b> Filtering issue which was removing valid  pedestrian & cyclist crashes resulting in further undercounting identified and corrected.<br>",
                              "<b>March 3:</b> February 2024 data updated and added to the dashboard.<br>",
                              "<b>April 9:</b> March 2024 data updated and added to the dashboard.<br>",
                              "<b>May 3:</b> April 2024 data updated and added to the dashboard.<br>",
                              "<b>June 5:</b> May 2024 data updated and added to the dashboard.<br>")
                       )
                )
              )),
      tabItem(tabName = "map_council",
              fluidRow(
                column(width = 3, 
                       checkboxGroupInput("type", "Person Type:", choices = unique(ped_cycle_df$Person_Type), selected = unique(ped_cycle_df$Person_Type)),
                       checkboxGroupInput("severity", "Injury Severity:", choices = unique(ped_cycle_df$Person_Injury_Severity), selected=unique(ped_cycle_df$Person_Injury_Severity)),
                       selectInput("year", "Year:", choices = c("All", unique(ped_cycle_df$Crash_Year)), selected="2023", multiple = TRUE)),
                column(width = 9,
                       leafletOutput("map", height = "95vh"))
              )
      ),
      tabItem(tabName = "map_bike",
              fluidRow(
                column(width = 3, 
                       checkboxGroupInput("severity_bike", "Injury Severity:", choices = unique(ped_cycle_df$Person_Injury_Severity), selected = unique(ped_cycle_df$Person_Injury_Severity)),
                       selectInput("year_bike", "Year:", choices = c("All", unique(ped_cycle_df$Crash_Year)), selected = "2023", multiple = TRUE)),
                column(width = 9,
                       leafletOutput("map_bikeFacilities", height = "95vh"))
              )
      ),
      tabItem(tabName = "data_desc",
              h2("Data Description"),
              p("All the data on crashes involving pedestrians and cyclists in San Antonio is collected from the Texas DOT Crash Record Information System (CRIS)
                Query tool. The data is filtered for crashes only occurring in San Antonio and extracted monthly."),
              p(""),
              p("The San Antonio City Council District boundaries were obtained from San Antonio's Open Data Portal and reflect the new redistricting that occurred
                 in 2022."),
              p(""),
              p("The bicycle routes were also obtained from San Antonio's Open Data Portal."),
              p("All the data can be found on my GitHub page.")),
      tabItem(tabName = "tech_notes",
              h2("Technical Notes"),
              p("This dashboard is built using R and several different R packages. R is a programming language for 
                statistical computing and graphics. The main package used to create this dashboard is shinydashboard. To create 
                the maps and City Council outlines, the leaflet package was used for mapping geographies and shapefiles.")),
      tabItem(tabName = "about_me",
              fluidRow(
                column(width = 8,
                       h2("About Me"),
                       p("Hello! My name is Jack Turek and I’m a data scientist who enjoys using data to help explain the world around me.
                My particular focus area is using data to evaluate public policies, specifically around urban issues – housing affordability, transportation equity, and urban spatial analysis.
                I’m also a huge sports fan, so you’re also just as likely to see a post diving into how the Chicago Cubs may be doing from a sabermetrics point of view or how my favorite soccer
                team may or may not make the Champions League."),
                       p("FYI: On the left hand side you'll see 'Buy Me a Pizza!'. You don't actually have to buy me a pizza, but if you like the 
                dashboard and want to contribute to the project and help keep the servers running, a small donation would be greatly 
                appreciated!"))))
    )
  )
)

server <- function(input, output, session) {
  library(tidyverse)
  library(ggrepel)
  library(leaflet)
  library(leaflet.extras)
  library(shiny)
  library(sp)
  library(sf)
  library(RColorBrewer)
  # Pedestrian/Cyclist crash data
  ped_cycle_df <- read.csv("ped_cycle_df.csv", header = TRUE, stringsAsFactors = FALSE)
  # Redistricted Council boundaries shapefile
  shapefile <- sf::st_read(dsn = "RedistrictedCouncilDistricts2022.shp") %>%
    sf::st_transform(crs = 4326)
  colorCount <- length(unique(shapefile$District))
  colorPalette <- brewer.pal(colorCount, "Paired")
  shapefile$ColorFactor <- factor(shapefile$District,
                                  levels = unique(shapefile$District))
  shapefile$color <- colorPalette[shapefile$ColorFactor]
  # Bike Facilities shapefile
  bike_shapefile <- sf::st_read(dsn = "Bike_Facilities.shp") %>%
    st_transform(crs = 4326)
  
  # Filter data for bicycle crash map
  ped_cycle_filter <- reactive({
    filtered_data <- ped_cycle_df[ped_cycle_df$Person_Type == "3 - PEDALCYCLIST", ]
    
    if (!"All" %in% input$severity_bike) {
      filtered_data <- filtered_data %>% 
        filter(Person_Injury_Severity %in% input$severity_bike)
    }
    if (!"All" %in% input$year_bike) {
      filtered_data <- filtered_data %>% 
        filter(Crash_Year %in% input$year_bike)
    }
    return(filtered_data)
  })
  
  # Filter data for the council district crash map based on user input
  council_crash_filter <- reactive({
    filtered_data <- ped_cycle_df
    
    if (length(input$type) < length(unique(ped_cycle_df$Person_Type))) {
      filtered_data <- filtered_data[filtered_data$Person_Type %in% input$type, ]
    }
    if (!"All" %in% input$severity) {
      filtered_data <- filtered_data %>% 
        filter(Person_Injury_Severity %in% input$severity)
    }
    if (!"All" %in% input$year) {
      filtered_data <- filtered_data %>% 
        filter(Crash_Year %in% input$year)
    }
    return(filtered_data)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.4936,
              lat = 29.4241,
              zoom = 11) %>%
      addPolygons(
        data = shapefile,
        fillColor = ~ color,
        weight = 2,
        opacity = 1,
        color = 'black',
        dashArray = '3',
        label = ~ District,
        labelOptions = labelOptions(noHide = TRUE),
        fillOpacity = 0.4
      ) %>%
      addCircleMarkers(
        data = council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),],
        lng = ~ Longitude,
        lat = ~ Latitude,
        color = 'black',
        fillColor = ~ ifelse(Person_Type == "3 - PEDALCYCLIST", "blue", "green"),
        radius = 3,
        weight = .5,
        opacity = .5,
        fillOpacity = 1,
        group = ~ Person_Type,
        popup = paste(
          "<strong>Person Type:</strong> ",
          council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Type,
          "<br>",
          "<strong>Crash Month: ",
          council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Crash_Month,
          "<br>",
          "<strong>Crash Year: ",
          council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Crash_Year,
          "<br>",
          "<strong>Street Number: ",
          council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Street_Number,
          "<br>",
          "<strong>Street Name: ",
          council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Street_Name,
          "<br>",
          "<strong>Person Injury Severity: ",
          council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Injury_Severity,
          "<br>",
          "<strong>Fatalities: ",
          council_crash_filter()[council_crash_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Death_Count,
          "<br>"
        )
      ) %>%
      
      
      addLegend(
        position = "topright",
        colors = c("blue", "green"),
        labels = c("Cyclist", "Pedestrian"),
        title = "Person Type"
      )
    
  })
  # Bike Facilities map
  output$map_bikeFacilities <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = -98.4936, lat = 29.4241, zoom = 11) %>%
      addPolylines(data = bike_shapefile,
                   color = 'red',
                   weight = 2.5,
                   dashArray = '2',
                   opacity = .8
      ) %>%
      addCircleMarkers(
        data = ped_cycle_filter()[ped_cycle_filter()$Person_Type == "3 - PEDALCYCLIST", ],
        lng = ~ Longitude,
        lat = ~ Latitude,
        color = 'black',
        fillColor = ~ ifelse(Person_Type == "3 - PEDALCYCLIST", "blue", "green"),
        radius = 3.8,
        weight = .5,
        opacity = .6,
        fillOpacity = 1,
        group = ~ Person_Type,
        popup = paste(
          "<strong>Person Type:</strong> ",
          ped_cycle_filter()$Person_Type, #[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Type,
          "<br>",
          "<strong>Crash Month: ",
          ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Crash_Month,
          "<br>",
          "<strong>Crash Year: ",
          ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Crash_Year,
          "<br>",
          "<strong>Street Number: ",
          ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Street_Number,
          "<br>",
          "<strong>Street Name: ",
          ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Street_Name,
          "<br>",
          "<strong>Person Injury Severity: ",
          ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Injury_Severity,
          "<br>",
          "<strong>Fatalities: ",
          ped_cycle_filter()[ped_cycle_filter()$Person_Type %in% c("3 - PEDALCYCLIST", "4 - PEDESTRIAN"),]$Person_Death_Count,
          "<br>"
        )
      )
  })
  
}

shinyApp(ui, server)



