# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

# selecting feature class

library(ENMeval)
library(terra)


##
##
#'linear=true',
#'quadratic=true',
#'hinge=true',
#'product=false', 
#'threshold=false', 
#'betamultiplier=3',

names <- list.files("./output/biasSWD/")
names <- gsub("biasSWD_", "", names)
names <- gsub(".csv", "", names)

# import occurrence SWD file

for (i in 1:length(names)){
  
  # occurrence SWD file
  pts.file <- paste0("./output/xySWD/xySWD_", names[i], ".csv")
  xySWD <- read.csv(pts.file)
  xySWD <- xySWD[,4:ncol(xySWD)]
  
  # background SWD file
  bg.file <- paste0("./output/biasSWD/biasSWD_", names[i], ".csv")
  bgSWD <- read.csv(bg.file)
  bgSWD <- bgSWD[,4:ncol(bgSWD)]
  
  if (nrow(xySWD) >= 5){
    eval <- ENMevaluate(occs = xySWD, bg = bgSWD, parallel = TRUE, numCores = 7,
                        tune.args = list(fc = c("L", "LQ", "LQH"), rm = 1:3),
                        algorithm = "maxent.jar", partitions = "randomkfold")
    
    # fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), rm = 1:3)                    
    # but common and recommended settings here are c(“L”, “LQ”, “LQH”). see Elith et al. 2011
    # RMvalues .-> A smoothing parameter. The higher the number the smoother your model.
    # Low values can lead to overfitting and low transferability to other times and spaces.
    
    #eval@results
    bestmod <- which(eval@results$AICc==min(eval@results$AICc, na.rm = TRUE))
    f <- eval@results[bestmod,]
    f[1:2]
    
    write.csv(f, paste0("./output/Fclass_best/ENMeval_best_", names[i], ".csv"))
    write.csv(eval@results, paste0("./output/Fclass_results/ENMeval_evalResults_", names[i], ".csv"))
    
  } else {
    
    print(paste0(names[i], " has fewer than 5 occurence records"))
  }
}

###############
########
##


