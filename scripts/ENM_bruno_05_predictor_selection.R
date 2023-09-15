# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

### select predictors to remove highly correlated variables

library(usdm)
library(dismo)
library(terra)


# upload species occurrence file
pts<- read.csv("./data/TableS1_SpeciesRecords.csv")
pts$sp <- gsub(" ", "_", pts$sp)
names <- unique(pts$sp)

# upload calibration predictors
### loop for NA mask for each species

for (i in 1:length(names)){
  
  pred_folder = paste0("./output/predictors/",names[i], "/")
  pred_path <-list.files(path = pred_folder, patter='*.asc$')
  pred_files <- paste(pred_folder,pred_path, sep="")
  
  r <- rast(pred_files)
  r <- c(r[[2:7]],r[[12:15]]) # keep only variables with biological meaning
                              # remove mean and quarters variables
  
  #bio2	= Mean Diurnal Range [°C]
  #bio3	= Isothermality
  #bio4	= Temperature Seasonality [standard deviation]
  #bio5	= Max Temperature of Warmest Month [°C*10]
  #bio6	= Min Temperature of Coldest Month [°C*10]
  #bio7	= Temperature Annual Range [°C*10]
  #bio12	= Annual Precipitation [mm/year]
  #bio13	= Precipitation of Wettest Month [mm/month]
  #bio14	= Precipitation of Driest Month [mm/month]
  #bio15	= Precipitation Seasonality [coefficient of variation]
  
  set.seed(2023)
  bg <- spatSample(r, 1000, "random", na.rm=TRUE, as.df=TRUE, exp = 1)

  # Select a variables subset with VIF < 10
  # and select the subset of variables
  v <- vifstep(bg, th = 10)
  
  c.file <- paste0("./output/vif/vif_cormatrix_", names[i], ".csv")
  write.csv(v@corMatrix, c.file, fileEncoding = "UTF-8")
  
  v.file <- paste0("./output/vif/vif_results_", names[i], ".csv")
  write.csv(v@corMatrix, v.file, fileEncoding = "UTF-8")
  
  # save only selected raster predictors
  n <- v@results[,1]
  p <- subset(r, n)
  
  out.dir <- paste0("./output/vif/pred_vif/", names[i], "/")
  dir.create(out.dir)
  writeRaster(p, paste0(out.dir,names(p),".asc"))

}

############
####
#


