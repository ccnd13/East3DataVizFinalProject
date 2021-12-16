library(shiny)
library(shinyWidgets)
library(sf)
library(rsconnect)
library(leaflet)
library(tidyverse)
library(NISTunits)
library(maptools)
library(rgeos)
library(data.table)

# Load data
df_street_lights<- read_csv("Street_Lights.csv")

park_locations<-'Parks_Locations_and_Features.csv'
df_park_locations<-read.csv(park_locations)

school_boundaries <- 'School_Boundaries.shp'
df_school_boundaries <- st_read(school_boundaries, stringsAsFactors = FALSE)

city_council <- 'City_Council_Districts.shp'
df_city_council <- st_read(city_council, stringsAsFactors = FALSE)

df_311_Contact_Management_Cases <- read_csv("311_Contact_Management_Cases.csv")

# Helper functions
distance <- function(lat1,lon1,lat2,lon2) {
  miles <- acos(cos(NISTdegTOradian(90-lat1))* # latitude 1
                  cos(NISTdegTOradian(90-lat2))+ # latitude 2
                  sin(NISTdegTOradian(90-lat1))* # latitude 1
                  sin(NISTdegTOradian(90-lat2))* # latitude 2
                  cos(NISTdegTOradian(lon1-lon2)))*3958.8 # longitude1 - longitude 2
  return(miles)
}

nearbyLights <- function(lat1, lon1, threshold) {
  sum(distance(lat1,
               lon1,
               df_street_lights$Lat,
               df_street_lights$Lon) < threshold)
}

# Data preprocessing
spatial_street_lights <- 
  df_street_lights %>%
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

spatial_park_locations <- 
  df_park_locations %>%
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

spatial_school_boundaries <-
  cbind(df_school_boundaries,
        st_centroid(df_school_boundaries$geometry) %>%
          st_coordinates()) %>%
  rename("Lon" = "X", "Lat" = "Y") %>%
  as.data.frame() %>%
  select(School, SchoolType, Lat, Lon) %>%
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

# Nearby lights to locations
spatial_park_locations$Nearby_Lights <-
  by(spatial_park_locations,
     seq_len(nrow(spatial_park_locations)),
     function(park) nearbyLights(park$Lat, park$Lon, 0.1)) %>%
  as.vector()

spatial_school_boundaries$Nearby_Lights <-
  by(spatial_school_boundaries,
     seq_len(nrow(spatial_school_boundaries)),
     function(school) nearbyLights(school$Lat, school$Lon, 0.1)) %>%
  as.vector()

# Add colors based on corresponding district
districtColors <- rainbow(6, s = 0.5)

spatial_street_lights$Color <- 
  st_intersects(st_transform(spatial_street_lights, 2163),
                st_transform(df_city_council$geometry, 2163),
                sparse = FALSE) %>%
  apply(MARGIN=1, function(row) {
    for(i in 1:6) {
      if(row[i]) {return(districtColors[i])}
    }
    return("white")
  }) %>%
  unlist()

df_city_council$Lights <-
  st_intersects(st_transform(df_city_council$geometry, 2163),
                st_transform(spatial_street_lights, 2163),
                sparse = FALSE) %>%
  apply(MARGIN = 1, sum)

# Add row for lights without district
df_city_council <- 
  df_city_council %>%
  add_row(Num = "None",
          Lights = nrow(spatial_street_lights) - sum(df_city_council$Lights))

# Find schools and parks outside defined districts
spatial_park_locations$InDistrict <- 
  st_intersects(st_transform(spatial_park_locations, 2163),
                st_transform(df_city_council$geometry, 2163),
                sparse = FALSE) %>%
  apply(MARGIN=1, function(row) {
    for(i in 1:6) {
      if(row[i]) {return(TRUE)}
    }
    return(FALSE)
  }) %>%
  unlist()

spatial_school_boundaries$InDistrict <- 
  st_intersects(st_transform(spatial_school_boundaries, 2163),
                st_transform(df_city_council$geometry, 2163),
                sparse = FALSE) %>%
  apply(MARGIN=1, function(row) {
    for(i in 1:6) {
      if(row[i]) {return(TRUE)}
    }
    return(FALSE)
  }) %>%
  unlist()

# Define popup
spatial_park_locations$popup <-
  paste("<b>", spatial_park_locations$Park_Name, "</b><br>",
        "Type: ", spatial_park_locations$Park_Type, "<br>",
        "Address: ", spatial_park_locations$Address, "<br>",
        #"Nearby Lights: ", spatial_park_locations$Nearby_Lights,
        sep ="")

spatial_school_boundaries$popup <-
  paste("<b>", spatial_school_boundaries$School, "</b><br>",
        "Type : ", spatial_school_boundaries$SchoolType, "<br>",
       # "Nearby Lights: ", spatial_school_boundaries$Nearby_Lights,
        sep ="")

df_city_council$popup <-
  paste("<b> District ", df_city_council$Num, "</b><br>",
        "Council Member : ", df_city_council$Council_Me, "<br>",
        "Member Email : ", df_city_council$Email, "<br>",
        "District Lights: ", df_city_council$Lights,
        sep ="")

# Marker icons
treeNightIcon <- makeIcon(
  iconUrl = "tree-night.png",
  iconWidth = 40,
  iconHeight = 40,
  iconAnchorX = 20,
  iconAnchorY = 0
)

treeErrorIcon <- makeIcon(
  iconUrl = "error-tree.png",
  iconWidth = 40,
  iconHeight = 40,
  iconAnchorX = 20,
  iconAnchorY = 0
)

schoolNightIcon <- makeIcon(
  iconUrl = "school-night.png",
  iconWidth = 40,
  iconHeight = 40,
  iconAnchorX = 20,
  iconAnchorY = 0
)

myOptions <- providerTileOptions(
  minZoom=12,
  maxZoom=16,
  zoomSnap=0.1,
  zoomDelta=0.1
)

ui <- fluidPage(
  titlePanel("Street Lights"),
  tabsetPanel(type = "tabs",
              tabPanel(
                "Parks and Schools",
                mainPanel(
                  leafletOutput("markersMap",
                                height = "80vh"),
                  width = 12
                )
              ),
              tabPanel(
                "Lights by District",
                mainPanel(
                  leafletOutput("districtsMap",
                                height = "80vh"),
                  width = 8
                ),
                sidebarPanel(
                  plotOutput("districtsBarplot",
                             height = "80vh"),
                  width = 4,
                  tags$style(".well {background-color:#FFFFFF;}")
                )
              ),
              tabPanel("Explore Features of Lights",
                       plotOutput("lightsBarplot",
                                  height = "80vh"),
                       width = 4,
                       tags$style(".well {background-color:#FFFFFF;}"),
              )
  )
)

#Create Wattage Groups
df_last_tab_lights <- copy(df_street_lights)
df_last_tab_lights$Wattage <- gsub('watts', '', df_last_tab_lights$Wattage)
df_last_tab_lights$Wattage <- gsub('Watts', '', df_last_tab_lights$Wattage)
df_last_tab_lights$Wattage <- gsub('Watts', '', df_last_tab_lights$Wattage)
df_last_tab_lights$Wattage <- gsub(' - ', '-', df_last_tab_lights$Wattage)
df_last_tab_lights$Wattage <- trimws(df_last_tab_lights$Wattage, which = c("both"))
df_last_tab_lights$Wattage[df_last_tab_lights$Wattage == "400"] <- "40-400"
df_last_tab_lights$Wattage[df_last_tab_lights$Wattage == "59"] <- "40-400"
df_last_tab_lights$Wattage[df_last_tab_lights$Wattage == "150"] <- "25-250"
df_last_tab_lights$Wattage[df_last_tab_lights$Wattage == "20- 200"] <- "20-200" 
df_last_tab_lights$Wattage[df_last_tab_lights$Wattage == "200"] <- "20-200"
df_last_tab_lights$Wattage[is.na(df_last_tab_lights$Wattage)] <- "Unknown"
df_last_tab_lights$Service[is.na(df_last_tab_lights$Service)] <- "Unknown"
df_last_tab_lights$Wattage[df_last_tab_lights$Wattage == "1000"] <- ">1000"
df_last_tab_lights$Wattage <- factor(df_last_tab_lights$Wattage, levels=c(">1000", "40-400", "25-250", "20-200", "17-175", "10-100", "Unknown"))
# Replace b by XXX
data     

server <- function(input, output) {
  # Tab 1
  output$markersMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter",
                       options = myOptions,
      ) %>% 
      addCircles(data = spatial_street_lights,
                 color = "wheat",
                 stroke = 0,
                 fillOpacity = 0.2,
                 radius = 30) %>%
      addMarkers(data = spatial_park_locations,
                 group = "Parks",
                 icon = treeNightIcon,
                 popup = ~popup) %>%
      addMarkers(data = spatial_school_boundaries,
                 group = "Schools",
                 icon = schoolNightIcon,
                 popup = ~popup) %>%
      addLayersControl(overlayGroups = c("Parks", "Schools"),
                       options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setMaxBounds(lng1 = -86.45, # left
                   lat1 = 41.8, # up
                   lng2 = -86.1, # right
                   lat2 = 41.5) %>% # down 
      htmlwidgets::onRender("
            function(el, x) {
                this.on('baselayerchange', function(e) {
                    e.layer.bringToBack();
                })
            }")
  })
  # Tab 2
  output$districtsMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter",
                       options = myOptions) %>% 
      addCircles(data = spatial_street_lights,
                 color = ~Color,
                 stroke = 0,
                 fillOpacity = .5,
                 radius = 30) %>%
      addPolygons(data = df_city_council,
                  color = districtColors,
                  fillOpacity = 0.1,
                  stroke = FALSE,
                  popup = ~popup) %>%
      addMarkers(data = filter(spatial_park_locations,
                               InDistrict == FALSE),
                 icon = treeErrorIcon,
                 popup = ~popup) %>%
      #            No cases of schools outside boundaries            
      #            addMarkers(data = filter(spatial_school_boundaries,
      #                                     InDistrict == FALSE),
      #                       icon = errorSchoolIcon,
      #                       popup = ~popup) %>%
      setMaxBounds(lng1 = -86.45, # left
                   lat1 = 41.8, # up
                   lng2 = -86.1, # right
                   lat2 = 41.5) %>% # down 
      htmlwidgets::onRender("
            function(el, x) {
                this.on('baselayerchange', function(e) {
                    e.layer.bringToBack();
                })
            }")
  })
  output$districtsBarplot <- renderPlot({
    ggplot(df_city_council,
           aes(y = Lights,
               x = Num)) +
      geom_bar(stat = "identity",
               fill = c(districtColors, "#858585"),
               color = "#090909") +
      labs(x="District") +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  })
# Tab 3
  output$lightsBarplot <- renderPlot({
    ggplot(df_last_tab_lights, aes(factor(Service),
                                   fill = factor(Wattage))) + scale_fill_manual(values = c("Unknown" = "blue","All Other" = "lightgrey")) +
      geom_bar() + xlab("Light Setting") + ylab("Count") + labs(fill = "Wattage") +
      theme_light()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
#deployApp()