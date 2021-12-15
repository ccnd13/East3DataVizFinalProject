library(shiny)
library(shinyWidgets)
library(sf)
library(rsconnect)
library(leaflet)
library(tidyverse)
library(NISTunits)
library(maptools)
library(rgeos)

# Load data
street_lights<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Street_Lights.csv'
df_street_lights<-read.csv(street_lights)

park_locations<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Parks_Locations_and_Features.csv'
df_park_locations<-read.csv(park_locations)

school_boundaries <- 'data/School_Boundaries/School_Boundaries.shp'
df_school_boundaries <- st_read(school_boundaries, stringsAsFactors = FALSE)

city_council <- 'data/City_Council_Districts/City_Council_Districts.shp'
df_city_council <- st_read(city_council, stringsAsFactors = FALSE)

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
          "Nearby Lights: ", spatial_park_locations$Nearby_Lights,
          sep ="")

spatial_school_boundaries$popup <-
    paste("<b>", spatial_school_boundaries$School, "</b><br>",
          "Type : ", spatial_school_boundaries$SchoolType, "<br>",
          "Nearby Lights: ", spatial_school_boundaries$Nearby_Lights,
          sep ="")

df_city_council$popup <-
    paste("<b> District ", df_city_council$Num, "</b><br>",
          "Council Member : ", df_city_council$Council_Me, "<br>",
          "Member Email : ", df_city_council$Email, "<br>",
          "District Lights: ", df_city_council$Lights,
          sep ="")

# Marker icons
treeNightIcon <- makeIcon(
    iconUrl = "icons/tree-night.png",
    iconWidth = 40,
    iconHeight = 40,
    iconAnchorX = 20,
    iconAnchorY = 0
)

treeErrorIcon <- makeIcon(
    iconUrl = "icons/error-tree.png",
    iconWidth = 40,
    iconHeight = 40,
    iconAnchorX = 20,
    iconAnchorY = 0
)

schoolNightIcon <- makeIcon(
    iconUrl = "icons/school-night.png",
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
    titlePanel("Street lights"),
    tabsetPanel(type = "tabs",
                tabPanel(
                    "Parks and schools",
                    mainPanel(
                        leafletOutput("markersMap",
                                      height = "80vh"),
                        width = 12
                    )
                ),
                tabPanel(
                    "Lights by district",
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
                )
    )
)

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
}

# Run the application
shinyApp(ui = ui, server = server)
# deployApp('../East3_FinalProject')