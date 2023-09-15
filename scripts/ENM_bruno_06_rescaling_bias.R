# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

# bias file
# cropping bias for calibration area
# filling bias file for predNA area extension
# re scaling bias

library(terra)

# load re scale function
#######################
rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x)
  if(is.null(x.max)) x.max = max(x)
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}
##########################

## load bias file generated in QGIS
b <- rast("./data/bias.tif")

## path of NA mask for all species
na_folder = "./output/predNA/"
na_path <-list.files(path = na_folder, patter='*.tiff$')
na_files <- paste(na_folder,na_path, sep="")

names <- gsub("./output/predNA/", "", na_files)
names <- gsub("_predNA.tiff", "", names)

for (i in 1:length(names)){
  
  maskNA <- rast(na_files[i])
  
  # resample bias to match maskNA layer
  bNA <- resample(b,maskNA)
  bNA <- bNA*maskNA
  
  # make maskNA with all cells equals to 0
  mask0 <- maskNA - 1
  
  # sum mask0 and bias file, "na.rm = T" will keep 0 in all cells with NA from bias file
  bias <- sum(mask0, bNA, na.rm = TRUE)
  
  # mask bias to the study region
  bias <- bias * maskNA
  
  #re-scale bias file using "rescale"function
  # set new minimum (new.min) and new maximum (new.max) for re-scale
  b.scale <- rescale(x = bias,
                     x.min = minmax(bias)[1], x.max = minmax(bias)[2],
                     new.min = 1, new.max = 100)

    writeRaster(b.scale, paste0("./output/bias/", names[i],"_bias.tiff"))
}

#############
#####
##
#

