#Load libraries and functions #################################################

#install.packages(c("shiny","leaflet","shinythemes","dplyr","ggplot2",
#                   "h2o","lubridate"))

library(shiny)
library(leaflet)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(lubridate)

#source("emulator.R")

# Get global variables #########################################
NLDAS2_GRID <- readRDS("_data/NLDAS2_GRID.rds")
keys <- readRDS("_data/spatial_coverage_keys.rds") %>% ungroup()
feature_names <- readRDS("_data/feature_names.rds")

library(h2o)
session.h2o <- h2o.init()

yield_gbm <- h2o.loadModel("_models/yield_CSMEnsemble_gbm")
dsoc_gbm <- h2o.loadModel("_models/dsoc_CSMEnsemble_gbm")

#7*400000*0.000247105


# USER INTERFACE ###########################
?shinytheme
ui <- fluidPage(theme = shinytheme("united"),
                h2(strong("Nimble-Switchgrass:"),em("Numerical Integration Model based on machine Learning of crop model Ensembles of bioenergy Switchgrass")),
                div(
                  p("This tool was developed by ",a("R. Martinez-Feria",href="mailto:mart2225@msu.edu"),
                    "and",a("B. Basso",href="mailto:basso@msu.edu"),", Dept. of Earth and Environmental Science, Michigan State Univerity, and supported in part by the US Department of Agriculture National Institute of Food and Agriculture (award #: 2019-67012-29595) "),
                  style ="margin: 20px"  
                ),
                div(
                  h3("Site location:"),
                  fixedRow(column(width = 2,numericInput("myLat","Latitude:",value = 42.40, step = 10^-2)),
                           column(width = 2,numericInput("myLon","Longitude:",value = -85.37, step = 10^-2))),
                  #style ="background-color: #ededed; border-color: #2e6da4; padding: 20px; margin: 20px",
                  leafletOutput("mymap")
                ),
                column(
                  inputPanel(
                    sliderInput("myPlantYear","Planting year",min = 1980,max = 2010,step = 1,value = 1990, round = F),
                    sliderInput("myYears","Growing seasons:",min = 5, max = 20, step = 1,value = 15),
                    sliderInput("myPlantingPop","Planting population (lb seeds per acre):",min = 5, max = 9, step = 0.1, value = 6.5),
                    sliderInput("myFertilizerRate","Nitrogen fertilizer (lb N per acre):",min = 0, max = 200, step = 1, value = 50),
                    sliderInput("myFertilizerDOY",
                                "Fertilizer applicaton dates:",
                                min = as.Date("2000-05-01","%Y-%m-%d"),
                                max = as.Date("2000-07-01","%Y-%m-%d"),
                                value=as.Date("2000-05-15"),
                                timeFormat="%m-%d"),
                    sliderInput("myHarvestDOY",
                                "Harvest Dates:",
                                min = as.Date("2000-09-01","%Y-%m-%d"),
                                max = as.Date("2000-12-01","%Y-%m-%d"),
                                value=as.Date("2000-10-01"),
                                timeFormat="%m-%d")
                  ),
                  width = 9
                  ),
                  column(
                    inputPanel(
                      actionButton("goPredict","Get prediction",
                                   icon("arrow-circle-down"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      radioButtons("mySave","Save data to disk?",c(FALSE,TRUE))
                    ),
                    width = 3
                ),
                fixedRow(column(width = 6,plotOutput(outputId = "p1")),
                         column(width = 6,plotOutput(outputId = "p2")))
                
)

# SERVER ###########################

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = input$myLon, lat = input$myLat, zoom = 13) %>%
      setMaxBounds( lng1 = input$myLon + 0.05,
                    lat1 = input$myLat + 0.05,
                    lng2 = input$myLon - 0.05,
                    lat2 = input$myLat - 0.05 ) %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery", options = providerTileOptions(minZoom = 10, maxZoom = 13)) %>%
      addTiles(group = "OSM (default)",
               options = providerTileOptions(minZoom = 10, maxZoom = 13)) %>%
      addRectangles(lat1=round(input$myLat,2)-0.005, lng1=round(input$myLon,2)-0.005,
                    lat2=round(input$myLat,2)+0.005, lng2=round(input$myLon,2)+0.005,color = "red",fill = "red") %>%
      addMarkers(lng = input$myLon, lat = input$myLat, options = markerOptions(draggable = F)) %>% 
      addLayersControl(
        baseGroups = c("OSM (default)","Esri Imagery"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addMiniMap(zoomLevelFixed = 4)
  })
  
  
  observeEvent(input$goPredict, {
    
    withProgress(message = 'Working...', value = 0, {
      
      # Step 1 (Down)Loading SSURGO data
      
      incProgress(1/4, detail = "Getting soil and weather data.")
      
      spatial_inputs <- readRDS(paste0("_data/spatial/",
                                       NLDAS2_GRID$NLDAS2_GRID[which.min(sqrt((NLDAS2_GRID$NLDAS2_GRID_Y  - input$myLat)^2 + (NLDAS2_GRID$NLDAS2_GRID_X  - input$myLon)^2))],
                                       ".rds"))
      
      keys_to_run <- keys %>% 
        filter(key %in% unique(spatial_inputs$key)) %>% 
        mutate(h = sqrt((lat  - input$myLat)^2 + (lon  - input$myLon)^2)) %>% 
        filter(h == min(h))
      
      toRun <- spatial_inputs %>% 
        filter(key %in% unique(keys_to_run$key)) %>% 
        mutate(PlantYear = input$myPlantYear,
               PlantingPop = input$myPlantingPop*400000*0.000247105,
               FertilizerDOY =  yday(input$myFertilizerDOY),
               HarvestDOY = yday(input$myHarvestDOY),
               HarvestNum = Year - PlantYear + 1,
               FertilizerRate = ifelse(HarvestNum <= 2, 0, input$myFertilizerRate)) %>% 
        filter(HarvestNum %in% 1:input$myYears)
      
      print(keys_to_run)
      if(length(toRun$key) > 0){
       

        incProgress(1/4, detail = "Running emulator")
        
        
        data_in.h20 <- as.h2o(toRun[,feature_names])
        data_out <- toRun %>%  
          bind_cols(data.frame(yield = as.data.frame(h2o.predict(yield_gbm, data_in.h20))$pred,
                               dsoc = as.data.frame(h2o.predict(dsoc_gbm, data_in.h20))$pred)) 
        
        
        incProgress(1/4, detail = "Displaying data")
        
        output$p1 <-renderPlot({
          
          data_out %>% 
            ggplot(aes(Year,yield/1.12/2,fill = bio12/25.4)) + 
            stat_summary(geom = "col", fun = mean, colour = "gray10", width = 1) + 
            stat_summary(geom = "pointrange",fun.min =  min, fun.max = max , fun = mean) + 
            scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5,"RdYlBu")) + 
            labs(y = "Predicted productivity (US tons per acre)",
                 x = "Year", fill = "Annual rainfall (inches)") + 
            coord_cartesian(expand = T) + 
            ggthemes::theme_few()
          
          
        }) 
        
        output$p2 <-renderPlot({
          
          data_out %>% 
            group_by(MUKEY) %>% 
            mutate(dsoc = c(0,cumsum(dsoc)[-length(dsoc)])) %>% 
            ggplot(aes(Year,dsoc/2.2)) + 
            stat_summary(aes(fill = "Range"), geom = "ribbon",fun.min =  min, fun.max = max , fun = mean, fill = "#d8b365") + 
            stat_summary(aes(fill = "Mean"), geom = "line", fun = mean, colour = "gray10") +
            stat_summary(aes(fill = "Mean"), geom = "point", fun = mean, shape = 21, size =4, fill = "#f5f5f5") +
            labs(y = "Predicted soil C sequestration\n(Metric tons C per acre)",
                 x = "Year", fill = "Annual rainfall (inches)") + 
            coord_cartesian(expand = T) + 
            ggthemes::theme_few()
          
        }) 
        
        
        
        if(input$mySave) {
          
          incProgress(1/5, detail = "Saving to disk...") 
          
          write.csv(data_out,paste0("_output/",
                                    input$myLat,"+",
                                    input$myLon,"+",
                                    input$myPlantYear,"-",input$myPlantYear+input$myYears,"+",
                                    "Fert=",input$myFertilizerRate,"+",
                                    "FertDOY=",yday(input$myFertilizerDOY),"+",
                                    "HarvDOY=",yday(input$myHarvestDOY),
                                    ".csv"
          ),row.names = F)
          
        } 
        
      } else {
        
        output$p1 <-renderPlot({
          
          qplot(1,1,geom="text",label = "No data available\nfor those coordinates") +
            theme_void()
          
          
        }) 
        
        output$p2 <-renderPlot({
          
          qplot(1,1,geom="text",label = "No data available\nfor those coordinates") +
            theme_void()
          
        }) 
        
        
      }
      
      
    })
    
  })
  
}



shinyApp(ui, server)
