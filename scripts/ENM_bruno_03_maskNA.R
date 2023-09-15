# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

## Making Raster with all NA values from all predictors raster files

library(terra)

# upload species occurence file
pts<- read.csv("./data/TableS1_SpeciesRecords.csv")
pts$sp <- gsub(" ", "_", pts$sp)
names <- unique(pts$sp)

# upload calibration predictors
### loop for NA mask for each species

for (i in 1:length(names)){
  
  pred_folder = paste0("./output/predictors/",names[i], "/")
  pred_path <-list.files(path = pred_folder, patter='*.asc$')
  pred_files <- paste(pred_folder,pred_path, sep="")
  
  pred <- rast(pred_files)

  # make some math to secure no cell has value equals zero
  # multiply all cells per 1000
  pred_m1 <- 1000 * pred
  
  # add 1 to each cell value
  pred_m2 <- 1 + pred_m1
  
  # check if we get rid of the zeros
  t <- which(as.data.frame(pred_m2) == 0)
  
  if (length(t) == 0){
    
    ###mask na
    # if yes, divide all layers by itself to get layers with value 1 for all cells with value
    pred_01 <- pred_m2/pred_m2
    
    # multiply all layers to have a single layer with NA in all layers
    predNA <- prod(pred_01,  na.rm = FALSE)
    
    plot(predNA, main = paste0("NA mask ", names[i]))
    writeRaster(predNA, paste0("./output/predNA/", names[i],"_predNA.tiff"))
    
  } else {
    sp <- paste0("check NA mask file for ", names[i])
    print(sp)
  }
}


#################
##########
####
