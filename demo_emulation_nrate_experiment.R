source("global.R")
#library(foreach)
# states <- c("iowa","nebraska","south dakota","north dakota","kansas","indiana",
#             "minnesota","missouri","wisconsin","michigan","ohio","illinois")
# 
# keys <- foreach(j = which(tolower(state.name[c(-2,-11)]) %in% states),
#                 .combine = rbind) %do% {
#   
#   readRDS(paste0("D:/Midwest_cropland_rasters/combinedKeys/",j,".rds")) %>%
#     mutate(key = substr(nldas_key,1,nchar(nldas_key) - 14))%>% 
#     group_by(lat,lon,key) %>%
#     summarise(area = sum(area))
#   
# }
# 
# saveRDS(keys,"data/spatial_coverage_keys.rds")
# 
# soils <- readRDS("../results/regional_sims_setup/soils.rds") %>%
#   arrange(SoilID, ZLYR) %>%
#   group_by(SoilID,ZLYR) %>%
#   mutate(z = list(top:(bottom - 1))) %>%
#   unnest()  %>%
#   filter(z  < 100) %>%
#   mutate(layer = ifelse(z > 25, 100,25)) %>%
#   transmute(layer,BD,Clay,Silt,PAWC = DUL - LL, OC, pH, SAT, KSAT = SWCN) %>%
#   gather(variable,y,BD:KSAT) %>%
#   mutate(variable = paste0(variable,"_",layer)) %>%
#   group_by(SoilID,variable) %>%
#   summarise(y = mean(y)) %>%
#   spread(variable,y)
# 
# uniqueKeys <- keys %>% 
#   ungroup() %>%
#   select(key)  %>% 
#   unique() %>%
#   mutate(state = str_sub(key,1,2),
#          key2 = str_replace(key, "_", "x")) %>% 
#   separate(key2,c("SoilID","StationID"),sep = "x") %>% 
#   left_join(soils) %>% 
#   filter(complete.cases(.))
# 
# # Plot spatial coverage
# keys %>%
#   ungroup() %>% 
#   filter(key %in% uniqueKeys$key) %>% 
#   group_by(lat,lon) %>% 
#   summarise(p = sum(area)) %>%
#   ggplot(aes(lon,lat)) + 
#   #geom_polygon(data= midwest.st, aes(x=long,y=lat,group=group),linetype = 1, lwd = 0.5, fill = "gray60") +
#   geom_raster(aes(fill = p), interpolate = F) #+
#   geom_path(data= midwest.st, aes(x=long,y=lat,group=group),
#             linetype = 1, lwd = 0.5, colour = "gray10")
# 
# bioclim <- readRDS("data/bioclim_allcoverage_hist.rds")
#  
# for(this.state in unique(uniqueKeys$state)){
#   print(this.state)
#   x <- uniqueKeys %>%
#     filter(state == this.state) %>%
#     mutate(PlantYear = round(runif(n(),min = 1980, max = 2000)),
#            PlantingPop = round(rnorm(n(),700,200))) %>% 
#     left_join(bioclim) %>%
#     filter(Year >= PlantYear, Year < PlantYear + 20) %>%
#     mutate(FertilizerDOY =  pmax(90,pmin(200,round(rnorm(n(),150,15)))),
#            HarvestDOY = pmax(210,pmin(350,round(rnorm(n(),290,20)))),
#            HarvestNum = Year - PlantYear + 1,
#            FertilizerRate = ifelse(HarvestNum <= 2, 0, 50))
# 
#   saveRDS(x, paste0("data/demo_predictions/",this.state,".rds"))
#   #write.csv(x, paste0("data/demo_predictions/",this.state,".csv"),row.names = F)
# 
# }

library(h2o)
session.h2o <- h2o.init(min_mem_size = "10G")

# Load simulated data for experimetal sites
feature_names <- readRDS("data/litSites_sims.rds") %>%
  filter(!is.na(obs), variable == "yield") %>%
  select(ExpID,obs,Ensemble,BD_100:HarvestNum) %>%
  names(.)

# Test yield
yield_gbm <- h2o.loadModel("models/yield_CSMEnsemble_gbm")
dsoc_gbm <- h2o.loadModel("models/dsoc_CSMEnsemble_gbm")

init_time <- Sys.time()

for(this.state in gsub(".rds","",list.files("data/demo_predictions/",pattern = "rds"))[-1]){
  
  print((this.state))
  
  data_in <- readRDS(paste0("data/demo_predictions/",this.state,".rds"))
  
  data_in.h20 <- as.h2o(data_in %>% select(feature_names[-1:-3]))
  
  data_out <- data_in %>% select(key,Year,HarvestNum)

    for(this.rate in 0:20*10) {
  
    data_in.h20[,"FertilizerRate"] <- this.rate
    
    data_out <- data_out %>%
      bind_cols(data.frame(x1 = as.data.frame(h2o.predict(yield_gbm, data_in.h20))$pred,
                          x2 = as.data.frame(h2o.predict(dsoc_gbm, data_in.h20))$pred) %>%
                  `names<-`(paste0(c("yield_N","dsoc_N"),this.rate)))
      
  }
    saveRDS(data_out,paste0("data/demo_predictions/N_rate_trials/",this.state,".rds"))

}

end_time <- Sys.time()

12*(end_time - init_time)/11

h2o.shutdown()







  