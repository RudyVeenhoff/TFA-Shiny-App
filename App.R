library(plotly)
library(shinydashboard)
library(lubridate)
library(forecast)

halfyearData <- read.csv("halfyearData.csv")
colNames <- gsub("^X","",colnames(halfyearData))
colNames <- as.numeric(colNames)

## used by fullNameOfDay()
daysof2011 <- seq(as.POSIXct("2011/1/1"),as.POSIXct("2011/12/31"),"day")

# returns the time, e.g. "13:45"
convertTime <- function(min){
  time <- paste0(min %/% 60, ":", min %% 60)
  format(strptime(time, format = "%H:%M"), "%H:%M")
}

### creating the a vector with all the clock times
timeVector <- NULL
for(i in 0:1439){
  timeVector <- c(timeVector,convertTime(i))
}

## returns the date e.g. "Thursday 27 October 2011"
fullNameOfDay <- function(n){
  format(daysof2011[n],"%A %d %B %Y")
}

yearDay <- function(n){
  yday(as.character(n))
}

## select day from DF
dayDF <- function(n){
  beginofday <- (n-1)*60*24+1
  endofday <- n*60*24
  halfyearData[beginofday:endofday,]
}

weekDF <-function(n){
  beginofweek <- (n-7)*60*24+1
  endofweek <- (n-1)*60*24
  halfyearData[beginofweek:endofweek,]
}


header <- dashboardHeader(title = "Traffic Forecasting App", titleWidth = 250)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Plot", icon=icon("line-chart"),
             menuSubItem("Speed vs Time", tabName="TvS"),
             menuSubItem("Speed vs Location", tabName="LvS"), 
             menuSubItem("Heatmap", tabName="heatmap")),
    menuItem("Forecast", tabName="fc", icon=icon("car")),
    menuItem("Documentation",icon=icon("info"),
             menuSubItem("About", tabName="info"),
             menuSubItem("Plot", tabName="infoPlot"), 
             menuSubItem("Forecasting", tabName="infofc")
             )
    
  )
)


body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "infoPlot", includeMarkdown("Plot.Rmd")),
    tabItem(tabName = "infofc", includeMarkdown("Forecast.Rmd")),
    tabItem(tabName = "info", includeMarkdown("Info.Rmd")),
    
    tabItem(tabName = "fc", 
            fluidRow(box(width = 12, plotlyOutput("plotfc"))),
            fluidRow(box(width = 4, 
                         title = "Input Date", solidHeader = TRUE, status = "info",
                         dateInput(inputId = "dateInputId4", label = NULL,
                                   value = "2011-01-08",
                                   min = "2011-01-08",
                                   max = "2011-6-31")),
                     box(width = 4,  
                         title = "Select Mile Marker", solidHeader = TRUE, status = "info",
                         uiOutput("selectInput2")),
                     box(width = 4, background = "navy",
                         actionButton("actionButton", 
                                      label = "Toggle Actual Speeds On/Off")))),
  
    tabItem(tabName = "TvS", fluidRow(box(width = 12, plotlyOutput("plottvs"))),
                             fluidRow(box(width = 4, 
                                          title = "Input Date", solidHeader = TRUE, status = "info",
                                          dateInput(inputId = "dateInputId1", label = NULL,
                                                    value = "2011-01-01",
                                                    min = "2011-01-01",
                                                    max = "2011-6-31")),
                                      box(width = 4,  
                                          title = "Select Mile Marker", solidHeader = TRUE, status = "info",
                                          uiOutput("selectInput")))),
    tabItem(tabName = "LvS", fluidRow(box(width = 12, plotlyOutput("plotlvs"))),
                             fluidRow(box(width = 4, height = "137", 
                                          title = "Input Date", solidHeader = TRUE, status = "info",
                                          br(),  
                                          dateInput(inputId = "dateInputId2", label = NULL,
                                                    value = "2011-01-01",
                                                    min = "2011-01-01",
                                                    max = "2011-6-31")),
                                      box(width = 4, height = "137", title = "Input Minutes", 
                                          solidHeader = TRUE, status = "info",
                                          sliderInput("minutes", label = NULL, 
                                                                  min = 0, 
                                                                  max = 1439,
                                                                  value = 0)))),
    tabItem(tabName = "heatmap", fluidRow(box(width = 12, plotlyOutput("plotheatmap"))),
                                 fluidRow(box(width = 4, 
                                              title = "Input Date", solidHeader = TRUE, status = "info",
                                              dateInput(inputId = "dateInputId3", label = NULL,
                                                        value = "2011-01-01",
                                                        min = "2011-01-01",
                                                        max = "2011-6-31"))))
                   
                          
                            
                      
       
                     
  )
)
  

  

ui = dashboardPage(header,sidebar,body)

#############################################

server = function(input, output){
  
  output$selectInput <- renderUI({
    
    selectInput("location", label = NULL, 
                choices = colNames, 
                selected = 1)  
  })
  
  output$selectInput2 <- renderUI({
    
    selectInput("location2", label = NULL, 
                choices = colNames) 
  })
  
  output$plottvs <- renderPlotly({
    
      n <- yearDay(input$dateInputId1)
      df <- dayDF(n)
      colNumber<- which(colNames == input$location)
      speeds <- df[,colNumber]
      numericSpeeds <- as.numeric(speeds)
      plottingData <- data.frame(Time=timeVector, Speed=numericSpeeds)
      p <- plot_ly(data = plottingData, x = ~Time) %>%
        add_lines(y = ~Speed, line = list(color = "#00526d", width = .5)) %>%
        layout(title = paste(fullNameOfDay(n),"@",input$location),
               xaxis= list(title = ""),
               yaxis= list(title= "Speed in km/h"),
               margin = list(b=60))
  })
    
  output$plotlvs <- renderPlotly({
    
      n <- yearDay(input$dateInputId2)
      df <- dayDF(n)
      minutes <- input$minutes
      speeds <- df[minutes+1,]
      numericSpeeds <- as.numeric(speeds)
      plottingData <- data.frame(Hectometerpaaltje=colNames, Speed=numericSpeeds)
      p <- plot_ly(data = plottingData, x = ~Hectometerpaaltje) %>%
        add_lines(y = ~Speed, line = list(color = "#2e9659", width = 3)) %>%
        layout(title = paste(fullNameOfDay(n), convertTime(minutes)),
               xaxis= list(title = ""),
               yaxis= list(title= "Speed in km/h"))
  })
    
output$plotheatmap <- renderPlotly({
      n <- yearDay(input$dateInputId3)
      df <- dayDF(n)
      x <- list(title = "The 143 measurement points on the A13")
      y <- list(title = "Minutes past midnight")
      p <- plot_ly(z=as.matrix(df), type="heatmap") %>%
        colorbar(title = "Speed in km/h") %>%
        layout(title = fullNameOfDay(n),xaxis = x,yaxis = y)
})
  
 
   
 output$plotfc <- renderPlotly({
   
      n <- yearDay(input$dateInputId4)
      colNumber <- which(colNames == input$location2)
      tsdata <- weekDF(n)
      tsdata <- tsdata[,colNumber]
      fit <- stlf(ts(tsdata[1:length(tsdata)], frequency=1440), h=1440)
      plottingData <- data.frame(Time=timeVector, PredictedSpeed=fit$mean,
                                 TrueValues = dayDF(n)[,colNumber])
    p <- plot_ly(data = plottingData, x = ~Time) %>%
      add_lines(y = ~PredictedSpeed, 
                line = list(color = "#2e9659", width = 1),
                name = "Forecast") %>%
      layout(title = paste(fullNameOfDay(n),"@",input$location2),
             xaxis= list(title = ""),
             yaxis= list(title= "Speed in km/h"),
             margin = list(b=60))
    if(input$actionButton %% 2 == 1){
      p <- p %>%
        add_lines(y = ~TrueValues, 
                  line = list(color = "#f9a65a", width = 1),
                  name="Observed Speed") %>%
        layout(legend = list(x = 0, y = 0))
    }
    p
  })
  
}

shinyApp(ui, server)
