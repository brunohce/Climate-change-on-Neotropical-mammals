# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

# generating background points with bias file

library(terra)

names <- list.files("./output/vif/pred_vif/")

for (i in 1:length(names)){
  
  # predictor files
  pred.folder <- paste0("./output/vif/pred_vif/", names[i])
  pred.files <- list.files(pred.folder, patter='*.asc$')
  pred <- rast(paste0(pred.folder,"/",pred.files))
  
  # bias file
  bias <- rast(paste0("./output/bias/",names[i],"_bias.tiff"))
  
  # all values from bias raster
  xy <- xyFromCell(bias, cells(bias)) # lat long
  c <- cells(bias) # cell number
  v <- values(bias, dataframe = T, na.rm = T) # values
  
  KDEpts <- cbind(c,xy,v) # combine all together
  colnames(KDEpts) <- c("cell", "x", "y", "prob")
  
  ## occurrence records
  occ <- read.csv(paste0("./output/pts_thin/xy_", names[i], ".csv"))
  
  ## extract cells number and lat long of occurrence records
  o <-extract(bias,occ[,5:6], cells = T, xy = TRUE)
  
  ## extract pred data for all occurrence records
  xySWD <- as.data.frame(extract(pred, o[,3]))
  species <- rep(names[i],nrow(o))
  xySWD <- cbind(species, o[,3:5], xySWD)
  write.csv(xySWD, paste0("./output/xySWD/xySWD_",names[i],".csv"))
  
  ## remove occurrence from background
  KDEpts <- subset(KDEpts, !is.element(KDEpts$cell, o$cell))
  
  ## extract pred data for all background points
  biasKDE_all <- as.data.frame(extract(pred, KDEpts[,1]))
  bg <- rep("background", nrow(KDEpts))
  biasKDE_all <- cbind(bg, KDEpts, biasKDE_all)
  envSWD <- biasKDE_all[,-5]
  write.csv(envSWD, paste0("./output/envSWD/envSWD_",names[i],".csv"))
  
  if (nrow(biasKDE_all) > 10000){
    ## sample background at bias probability
    biasSWD <- biasKDE_all[sample(seq(1:nrow(biasKDE_all)), # sample from biasKDE_all rows
                                  size=10000, # sample 10000 points
                                  replace=F, # without replacement
                                  prob=biasKDE_all[,"prob"]), # sample in a probability equals values at column "prob"
                           c(1:4,6:ncol(biasKDE_all))] # sample columns 1 to 4 and 6 to the end, all except column 5 "prob")
    
    write.csv(biasSWD, paste0("./output/biasSWD/biasSWD_",names[i],".csv"))
    
  } else {
    print (paste0(names[i], " has fewer than 10000 background points"))
    n <- round(nrow(biasKDE_all)*.8, digits = 0)
    
    biasSWD <- biasKDE_all[sample(seq(1:nrow(biasKDE_all)), # sample from biasKDE_all rows
                                  size=n, # sample 10000 points
                                  replace=F, # without replacement
                                  prob=biasKDE_all[,"prob"]), # sample in a probability equals values at column "prob"
                           c(1:4,6:ncol(biasKDE_all))] # sample columns 1 to 4 and 6 to the end, all except column 5 "prob")
    
    write.csv(biasSWD, paste0("./output/biasSWD/biasSWD_",names[i],".csv"))
    
  }
}

###############
#########
##

