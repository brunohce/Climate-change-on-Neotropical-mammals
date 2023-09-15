# ENM ms bruno
# by Ana Carolina Loss
# 15 AGO 2023

### Area calculation and summary


library(terra)

# scenario names
sc <- list.files("./output/binary_maps/", full.names = FALSE)


# make empty data frame to store TSS and AUC values for all runs
area_df <- as.data.frame(matrix(ncol = 8, nrow = 0))
colnames(area_df)<-c("Scenario","Species","Sqkm", "%Change", "Area ratio", "Sqkm_log", "%Change_log", "Area ratio_log")


for (k in 1) { # current scenario
  
  # species with binary distribution maps
  spp.name <- list.files(paste0("./output/binary_maps/", sc[k], "/"), patter = '*_dist_1NA.tif$', full.names = FALSE)
  
  
  for (i in 1:length(spp.name)){
    dist.path <- paste0("./output/binary_maps/", sc[k],"/", spp.name[i])
    bin <- rast(dist.path)
    area <- expanse(bin, unit="km")
    
    area <- area[,2]
    area.c <- area
    change <- 100*((area/area.c)-1)
    ratio <- area/area.c
    sp <- gsub("_dist_1NA.tif", "", spp.name[i])
    
    area.l <- log(area)
    area.cl <- area.l
    change.l <- 100*((area.l/area.cl)-1)
    ratio.l <- area.l/area.cl
    
    df <- cbind(sc[k], sp, area, change, ratio, area.l, change.l, ratio.l)
    colnames(df) <- c("Scenario","Species","Sqkm", "%Change", "Area ratio", "Sqkm_log", "%Change_log", "Area ratio_log")
    
    
    area_df <- rbind(area_df, df)
    #colnames(area_df) <-c("Scenario","Species","Sqkm", "%Change", "Area ratio")
    
    # calculate area for each scenario and species
  }

} # end of loop 



for (k in 2:length(sc)) {
  
  # species with binary distribution maps
  spp.name <- list.files(paste0("./output/binary_maps/", sc[k], "/"), patter = '*_dist_1NA.tif$', full.names = FALSE)
  
  for (i in 1:length(spp.name)){
    dist.path <- paste0("./output/binary_maps/", sc[k],"/", spp.name[i])
    bin <- rast(dist.path)
    area <- expanse(bin, unit="km")
    area <- area[,2]
    area.c <- as.numeric(area_df[i,3])
    change <- 100*((area/area.c)-1)
    ratio <- area/area.c
    sp <- gsub("_dist_1NA.tif", "", spp.name[i])
    
    area.l <- log(area)
    area.cl <- as.numeric(area_df[i,6])
    change.l <- 100*((area.l/area.cl)-1)
    ratio.l <- area.l/area.cl
    
    df <- cbind(sc[k], sp, area, change, ratio, area.l, change.l, ratio.l)
    colnames(df) <- c("Scenario","Species","Sqkm", "%Change", "Area ratio", "Sqkm_log", "%Change_log", "Area ratio_log")
    
    area_df <- rbind(area_df, df)
    
  }
  
} # end looop for scenarios



write.csv(area_df, file = "./output/area/area_comparison.csv")




######################
############
###


