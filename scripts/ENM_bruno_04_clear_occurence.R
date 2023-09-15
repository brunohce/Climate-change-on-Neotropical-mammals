# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

### Check for occurrence records with no data for any predictor
### Keep only one record per cell

library(terra)

## predictors NA file
pred_folder = paste0("./output/predNA/")
pred_path <-list.files(path = pred_folder, patter='*.tiff$')
pred_files <- paste(pred_folder,pred_path, sep="")

names <- gsub("./output/predNA/", "", pred_files)
names <- gsub("_predNA.tiff", "", names)
#names <- gsub("_", " ", names)

## occurrence data
xy_all<- read.csv("./data/TableS1_SpeciesRecords.csv")
xy_all$sp <- gsub(" ", "_", xy_all$sp)

#xy_all <- xy_all[,c("Nome.científico", "Long_SDM", "Lat_SDM")]

for (i in 1:length(pred_files)){
  predNA <- rast(pred_files[i])
  xy <- xy_all[xy_all$sp==names[i],]
  pts <- vect(xy, crs = "epsg:4326", geom = c("lon", "lat"))
  
  pts$mask <- extract(predNA, pts)[,2] # extract raster values for each occurrence
  pts$cell <- cells(predNA, pts)[,2] # extract raster cell number for each occurrence
  pts <- pts[!duplicated(pts$cell),] # keep only one record per cell
  
  pts_out <- pts[is.na(pts$mask), ] # identify records with NA values for predictors
  ptsNA <- as.data.frame(pts_out)
  sp.name <- names[i]
  file_name <- paste0("./output/ptsNA/ptsNA_", sp.name, ".csv")
  write.csv(ptsNA, file_name, fileEncoding = "UTF-8")
  
  pts <- pts[!is.na(pts$mask),] # remove occurrence with no predictor value
  coords <- crds(pts)
  pts_xy <- as.data.frame(pts)
  pts_xy <- cbind(pts_xy,coords)
  
  if (nrow(pts_xy) >= 3){
    file_name <- paste0("./output/pts_thin/xy_", sp.name, ".csv")
    write.csv(pts_xy, file_name, fileEncoding = "UTF-8") 
  } else {
    paste0(sp.name, " has less than 3 records. Not to be modeled.")
    
  }
  
}

#################
#########
###
