# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

options(java.parameters = "-Xmx8g")

library(dismo)


# lat/long for all species to be modeled
spp.xy <- read.csv("./data/TableS1_SpeciesRecords.csv")
spp.xy$sp <- gsub(" ", "_", spp.xy$sp)

# species names
spp <- unique(spp.xy$sp)

# climate scenarios
sc <- c("current","ssp126", "ssp245", "ssp370" , "ssp585")

for (j in 1:length(sc)) {
  
  # predictor files for projecting
  pred.files <- list.files(paste0("./output/predictors/wc21/", sc[j], "/"), patter='*.asc$', full.names = TRUE)
  pred.all <- stack(pred.files)

  #####
  for (i in 1:length(spp)){

    #Output folder
    dir.name <- paste0("./output/models/", sc[j], "/", spp[i], "/")
    dir.create(dir.name)
    
    #SWDpts
    xy.path <- paste0("./output/xySWD/xySWD_",spp[i],".csv")
    pts <- read.csv(xy.path)
    pts <- pts[,6:length(pts)]
    #pts.v <- rep(1, nrow(pts))
    
    #SWDenv
    env.path  <-paste0("./output/biasSWD/biasSWD_",spp[i],".csv")
    env <- read.csv(env.path)
    env <- env[,6:length(env)]
    #env.v <- rep(0,nrow(env))
    
    #SWD all data
    swd <- rbind(pts,env)
    #all.v <- c(pts.v,env.v)
    swd.v <- c(rep(1, nrow(pts)), rep(0,nrow(env)))
    
    #predictors for projection
    p.names <- colnames(swd)
    pred <- subset(pred.all, p.names)
    
    ### arguments
    # Fclass
    f.path <- paste0("./output/Fclass_best/ENMeval_best_",spp[i],".csv")
    f <- read.csv(f.path)
    fc <- f$fc
    
    if (fc =='L'){
      fclass <-c("linear=true","quadratic=false","hinge=false","product=false","threshold=false")
    } else {
      if (fc =='LQ'){
        fclass <-c("linear=true","quadratic=true","hinge=false","product=false","threshold=false")
      } else {
        fclass <-c("linear=true","quadratic=true","hinge=true","product=false","threshold=false")
      }
    }
    
    # Regularization multiplier
    rm <-paste0("betamultiplier=",f$rm)
    
    args = c('randomseed=true',
             'outputformat=logistic',
             'replicatetype=crossvalidate',
             'replicates=5',
             #'threads=60',
             fclass,
             rm,
             'writebackgroundpredictions=true')
    
    
    #fit model
    model <- maxent(x=swd, p=swd.v, path = dir.name, args)
    
    # predict model
    p <- predict(object = model, x = pred, filename = dir.name,
                 na.rm = TRUE, format = 'GTiff', overwrite = TRUE, progress = 'text')
  }
  
  
} # end loop j for sc
###########
#######
###

