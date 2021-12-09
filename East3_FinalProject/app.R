#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)

# setwd("C:\\Users\\msisk1\\Documents\\GIT\\Teaching\\--Data-Viz-2021-Fall\\Week04\\LiveSession\\East_Demo")


elections <- st_read("ElectionResultsByState.shp")
pal <- colorFactor(palette = c("blue","red"), domain = c("D", "R"))

elections.data <- elections %>% st_set_geometry(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Elections Viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year",label = "Choose a Year",choices = names(elections[10:67]%>%st_set_geometry(NULL)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "AmandaPlot",
                 plotOutput(outputId = "piePlot")
                 
                 
        ),
        tabPanel(title = "CraigPlot",
                 plotOutput(outputId = "piePlot")
                 
                 
        ),#end tabpanel Pie
        tabPanel(title = "CesarPlot",
                 leafletOutput(outputId = "map")
        )
      )#end tabset
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  elec.subset <- reactive({
    elections.data[,input$year]
  })
  output$piePlot <- renderPlot({
    print(elections[,input$year])
    pie(table(elec.subset()))
  })
  output$map <- renderLeaflet({
    leaflet()%>%
      addTiles()%>%
      addPolygons(data =elections, color = ~pal(elec.subset()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
