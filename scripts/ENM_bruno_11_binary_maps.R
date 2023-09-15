# ENM ms bruno
# by Ana Carolina Loss
# 15 AGO 2023

###### Distribution range maps
## binary maps from models
## buffer


library(terra)
#library(xlsx)
library(dplyr)


## occurrence data
xy_all<- read.csv("./data/TableS1_SpeciesRecords.csv")
xy_all$sp <- gsub(" ", "_", xy_all$sp)

# species names and metrics to built binary maps from models
metric_average <- read.csv("./output/evaluation/01_validation_average.csv", row.names = 1)
spp.name <- unique(metric_average$species)
sc <- as.vector(unique(metric_average$scenario))

for (k in 1:length(sc)){
  
  for (i in 1:length(spp.name)){
    # averaged map
    sdm_avg <- rast(paste0("./output/models/",sc[k], "/", spp.name[i], "/", spp.name[i],"_cbi.tif"))
    
    # selected threshold averaged
    threshold10 <- as.numeric(subset(metric_average, 
                                     grepl(spp.name[i],metric_average$species) & grepl(sc[k],metric_average$scenario),
                                     select = Bin.Prob))
    
    # generate binary map
    sdm_bin <- sdm_avg >= threshold10
    #plot(sdm_bin)
    #sdm_bin <- project(sdm_bin, "EPSG:4326")
    
    ### to avoid over prediction, clip binary map using mcp from occurrence records
    # occurrence records
    xy <- xy_all[xy_all$sp==spp.name[i],]
    pts <- vect(xy, crs = "epsg:4326", geom = c("lon", "lat"))
    
    # make mcp
    mcp <- convHull(pts)
    b <- buffer(mcp, 200000) # 200km buffer
    m <- mask(sdm_bin,b) # clip sdm binary models to buffer extent
    writeRaster(m, filename= paste0("./output/binary_maps/", sc[k], "/", spp.name[i],"_dist_bin.tif"), overwrite=T)
    
    # keep only pixels with presence
    m1 <- m/m
    #plot(m1)
    writeRaster(m1, filename= paste0("./output/binary_maps/", sc[k], "/", spp.name[i],"_dist_1NA.tif"), overwrite=T)
    
  }
  
} # end of loop for scenarios



############################
##################
###


