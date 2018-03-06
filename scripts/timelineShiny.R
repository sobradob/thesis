# Shiny app which creates timeline of one data set

timelineMap <- function(data){
  
  library(shiny)
  library(leaflet)
  library(dplyr)
  
  # Define UI for application that draws a ma
  minData<- min(data$time)
  maxData<- max(data$time)
  
  shinyApp(
    # add UI
    ui <- bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("mapAct", width = "100%", height = "100%"),
      absolutePanel(top = "10", right = "20",
                    sliderInput("animation", "Time:",
                                min = minData,
                                max = maxData,
                                value = minData+1,
                                timezone = "+0200",
                                animate =
                                  animationOptions(interval = 600, loop = TRUE))
      )
    ),# Define server
    server <- function(input, output) {
      #stuff in server
      
      rollWindowSecs <- 180
      
      rollingData <- reactive({
        from<- input$animation-rollWindowSecs
        till<- input$animation+rollWindowSecs
        data %>% filter(time >= from & time <=  till)
      })
      
      priorData <- reactive({
        till<- input$animation-rollWindowSecs
        data %>% filter( time <=  till)
      })
      
      output$mapAct<-renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addProviderTiles(providers$CartoDB.Positron)%>%
          fitBounds(lng1 = min(data$lon),lat1 = min(data$lat),lng2 = max(data$lon),lat2 = max(data$lat))# set to reactive minimums
      })
      
      observe({
        leafletProxy("mapAct") %>%
          clearShapes() %>%
          addCircles(lng = ~lon, lat = ~lat,
                     radius = ~accuracy, opacity = 0.1,fillOpacity = 0.01,color = "#DF2935",
                     data = priorData()) %>%
          addCircles(lng = ~lon, lat = ~lat,
                     radius = ~accuracy, opacity = 0.8,fillOpacity = 0.8,color = "#e4ef40",
                     data = rollingData())
      })
    },options = list(height = 500)
  )
}
