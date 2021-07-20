#################################################################
#Example of how to run the emulator for multiple locations ######
#################################################################
library(tidyverse)
library(foreach)
library(h2o)
# Data extent ##################################################
coverage <- readRDS("_data/spatial_coverage_keys.rds") %>% 
  group_by(lat,lon) %>% 
  summarise(area = sum(area))
# Each file contains data available for a 0.01 degree resolution pixel 

midwest.st <- map_data("state") %>%
  filter(region %in% c("iowa","nebraska","south dakota","north dakota","kansas","indiana",
                       "minnesota","missouri","wisconsin","michigan","ohio","illinois"))

coverage %>% ggplot(aes(lon,lat)) + 
  geom_tile(aes(fill = area)) + 
  geom_path(data= midwest.st, aes(x=long,y=lat,group=group),
            linetype = 1, lwd = 0.5, colour = "gray10") +
  coord_fixed(ratio = 1.3) + 
  scale_fill_viridis_c() + 
  labs(fill = "Agricultural Soils (ha)", x = "Longitude", y = "Latitude") + 
  guides(fill = guide_colourbar(title.position = "top", direction = "horizontal", barwidth = 10, barheight = 0.5)) +
  theme_classic() + theme(legend.position = c(0.8,0.9))

ggsave("coverage.png",width = 6, height = 4.6)

#Sample random location from the weather grid #########################
spatial_inputs <- foreach(i = sample(list.files("_data/spatial/",full.names = T),100,replace = F), .combine = rbind) %do% {readRDS(i)}

this.coverage <- readRDS("_data/spatial_coverage_keys.rds") %>% 
  ungroup() %>% 
  filter(key %in% unique(spatial_inputs$key)) %>% 
  separate(key,c("soil","lat1","lon2"),sep = "_") %>% 
  group_by(lat1,lon2) %>% 
  summarise(lat = mean(lat),
            lon = mean(lon),
            area = sum(area))

this.coverage %>% ggplot(aes(lon,lat)) + 
  geom_point(aes(size = area), shape = 21, fill = "red", alpha = 0.5) + 
  geom_path(data= midwest.st, aes(x=long,y=lat,group=group),
            linetype = 1, lwd = 0.5, colour = "gray10") +
  coord_fixed(ratio = 1.3) + 
  labs(size = "Selected area (ha)", x = "Longitude", y = "Latitude") + 
  theme_classic()

#Add Management to inputs ######################################
myYears <- 15 #Number of years to run (5 to 20) 
myPlantYear <- 1990 #Year of start of emulation (1980-2010)
myPlantingPop <-700 #Plainting population in plants/m2 (300-1200)
myHarvestDOY <- 290 #Day of year of application of fertilizer (210-350)
myFertilizerDOY <- 150 #Day of year of application of fertilizer (90-200)
myFertilizerRate <- 50 #Nitrogen fertilizer rate in kg N/ha (0-200)

toRun <- spatial_inputs %>% 
     mutate(PlantYear = myPlantYear,
            PlantingPop = myPlantingPop,
            FertilizerDOY =  myFertilizerDOY,
            HarvestDOY = myHarvestDOY,
            HarvestNum = Year - PlantYear + 1,
            FertilizerRate = ifelse(HarvestNum <= 2, 0, myFertilizerRate)) %>% 
  filter(HarvestNum %in% 1:myYears)

# Load emulator models #########################################
session.h2o <- h2o.init()

yield_gbm <- h2o.loadModel("_models/yield_CSMEnsemble_gbm")
dsoc_gbm <- h2o.loadModel("_models/dsoc_CSMEnsemble_gbm")
feature_names <- readRDS("_data/feature_names.rds") #colunms needed by emulator

# Send data to cluseter ########################################
data_in.h20 <- as.h2o(toRun[,feature_names])

# Run predictions ##############################################
data_out <- toRun %>%  
  bind_cols(data.frame(yield = as.data.frame(h2o.predict(yield_gbm, data_in.h20))$pred, #Agronomic yields in Mg/ha
                       dsoc = as.data.frame(h2o.predict(dsoc_gbm, data_in.h20))$pred #Annual SOC change in Mg C/ha 
                       )) 

# Vizualize predictions ##############################################################
data_out %>% 
  ggplot(aes(Year,yield)) +
  geom_line(aes(group = key), colour = "red", alpha = 0.1) + 
  stat_summary(geom = "line", fun = mean, colour = "gray10", lwd = 1) +
  labs(y = "Predicted yield (Metric tons per ha)",
       x = "Year") 

data_out %>% 
  group_by(key) %>% 
  mutate(dsoc = c(0,cumsum(dsoc)[-myYears])) %>% 
  ggplot(aes(Year,dsoc)) + 
  geom_line(aes(group = key), colour = "red", alpha = 0.1) + 
  stat_summary(geom = "line", fun = mean, colour = "gray10", lwd = 1) +
  labs(y = "Predicted soil C sequestration\n(Metric tons C per ha)",
       x = "Year")
