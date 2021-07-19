library(tidyverse)



#Get global variables 
NLDAS2_GRID <- readRDS("_data/NLDAS2_GRID.rds")
keys <- readRDS("_data/spatial_coverage_keys.rds") %>% ungroup()
feature_names <- readRDS("_data/feature_names.rds")

library(h2o)
session.h2o <- h2o.init()

yield_gbm <- h2o.loadModel("_models/yield_CSMEnsemble_gbm")
dsoc_gbm <- h2o.loadModel("_models/dsoc_CSMEnsemble_gbm")


myLat <- 42.985843585677834
myLon <- -93.5093908622644
myPlantYear <- 1990
myFertilizerDOY <- 150
myHarvestDOY <- 290
myFertilizerRate <- 50
myPlantingPop <-700

spatial_inputs <- readRDS(paste0("_data/spatial/",
                                 NLDAS2_GRID$NLDAS2_GRID[which.min(sqrt((NLDAS2_GRID$NLDAS2_GRID_Y  - myLat)^2 + (NLDAS2_GRID$NLDAS2_GRID_X  - myLon)^2))],
                                 ".rds"))

keys_to_run <- keys %>% 
  filter(key %in% unique(spatial_inputs$key)) %>% 
  mutate(h = sqrt((lat  - myLat)^2 + (lon  - myLon)^2)) %>% 
  filter(h == min(h))

toRun <- spatial_inputs %>% 
  filter(key %in% unique(keys_to_run$key)) %>% 
     mutate(PlantYear = myPlantYear,
            PlantingPop = myPlantingPop,
            FertilizerDOY =  myFertilizerDOY,
            HarvestDOY = myHarvestDOY,
            HarvestNum = Year - PlantYear + 1,
            FertilizerRate = ifelse(HarvestNum <= 2, 0, myFertilizerRate)) %>% 
  filter(HarvestNum %in% 1:20)
  

data_in.h20 <- as.h2o(toRun[,feature_names])
data_out <- toRun %>%  
  bind_cols(data.frame(yield = as.data.frame(h2o.predict(yield_gbm, data_in.h20))$pred,
                       dsoc = as.data.frame(h2o.predict(dsoc_gbm, data_in.h20))$pred)) 
            ?stat_summary
data_out %>% 
  ggplot(aes(Year,yield/1.12/2,fill = bio12/25.4)) + 
  stat_summary(geom = "col", fun = mean, colour = "gray10", width = 1) + 
  stat_summary(geom = "pointrange",fun.min =  min, fun.max = max , fun = mean) + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5,"RdYlBu")) + 
  labs(y = "Predicted productivity (US tons per acre)",
       x = "Year", fill = "Annual rainfall (inches)") + 
  coord_cartesian(expand = F) + 
  ggthemes::theme_few()

data_out %>% 
  group_by(MUKEY) %>% 
  mutate(dsoc = c(0,cumsum(dsoc)[-20])) %>% 
  ggplot(aes(Year,dsoc/2.2)) + 
  stat_summary(geom = "ribbon",fun.min =  min, fun.max = max , fun = mean, fill = "#d8b365") + 
  stat_summary(geom = "line", fun = mean, colour = "gray10", width = 1) +
  stat_summary(geom = "point", fun = mean, shape = 21, size =4, fill = "#f5f5f5") +
  labs(y = "Predicted soil C sequestration\n(Metric tons C per acre)",
       x = "Year", fill = "Annual rainfall (inches)") + 
  coord_cartesian(expand = F) + 
  ggthemes::theme_few()



