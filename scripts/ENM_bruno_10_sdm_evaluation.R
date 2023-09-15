# ENM ms bruno
# by Ana Carolina Loss
# 14 AGO 2023

### Models validation metrics (AUC and TSS)
### Average valid models

library(terra)
library(xlsx)
library(enmSdmX)


#################################################
### Models validation metrics (AUC, TSS, CBI) ####
##################################################
# For TSS and CBI values calculation "Write background predictions" has to be enabled in Maxent

# make an empty list to store all TSS and AUC results, for each scenario
eval_summary <- list()

# make empty data frame to store TSS and AUC values for all runs
eval_df <- as.data.frame(matrix(ncol = 8, nrow = 0))
colnames(eval_df)<-c("scenario","species","Replica", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI")


# climate scenarios
sc <- c("current","ssp126", "ssp245", "ssp370" , "ssp585")

### PATH to output data

for (k in 1:length(sc)) {
  
  # output folder
  # list directory path for folders
  out.dir <- list.files(paste0("./output/models/", sc[k]), full.names = TRUE)
  spp.name <- list.files(paste0("./output/models/", sc[k]), full.names = FALSE)
  
  # loop for calculating evaluation metrics for all species, replicates
  for (a in 1:length(spp.name)){
    # list output files
    path <- paste0(out.dir[a], "/")
    # list all replicates
    listout <- list.files(path, pattern="_samplePredictions.csv")
    listout <- sub("_samplePredictions.csv","",listout)
    
    ## make empty data.frame to store evaluation values
    validation_general <- as.data.frame(matrix(ncol = 8, nrow = 0))
    colnames(validation_general)<-c("scenario","species","Replica", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI")
    
    for (i in 1:length(listout)){
      
      sp <- listout[i]
      presence<-read.csv(paste(path,sp,"_samplePredictions.csv",sep=""))
      background<-read.csv(paste(path,sp,"_backgroundPredictions.csv",sep=""))
      
      # prediction at presences and background sites
      predPres <- presence$Logistic.prediction
      predBg <- background$Logistic
      
      # read maxent output
      maxres <- read.csv(paste(path,"maxentResults.csv", sep = ""))
      auc <-maxres[maxres[,1]==sp,"Test.AUC"] # AUC from maxent output
      thisTr<- maxres[maxres[,1]==sp,"Maximum.test.sensitivity.plus.specificity.Logistic.threshold"]
      thisTss <- enmSdmX::evalTSS(pres=predPres, contrast=predBg, thresholds = thisTr)
      thisAuc <- enmSdmX::evalAUC(pres=predPres, contrast=predBg)
      thisCbi <- enmSdmX::evalContBoyce(pres=predPres, contrast=predBg)
      
      # combine all values
      evaldf <- as.data.frame(sc[k])
      evaldf[2] <- spp.name[a]
      evaldf[3] <- sp
      evaldf[4] <- thisTr
      evaldf[5] <- auc
      evaldf[6] <- thisAuc
      evaldf[7] <- thisTss
      evaldf[8] <- thisCbi
      colnames(evaldf) <- c("scenario","species","Replica", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI")
      
      # bind it to final data frame
      validation_general <- rbind(validation_general,evaldf)
    }
    
    eval_summary[[a]] <- validation_general
    # collapse list in one big data.frame
    eval_scenario <- do.call(rbind,eval_summary)
  }
  
  eval_df <- rbind(eval_df,eval_scenario)
  
} # end of loop for scenarios


### summary evaluation metrics data for all species and scenarios

# Classify replicates according to selected threshold for AUC (>= 0.50),  TSS (>=0.0) and CBI (>= 0.4)
eval_df$decision <- ifelse(eval_df$wAUC >= 0.5 & eval_df$TSS >= 0.0 & eval_df$CBI >= 0.4, "include", "remove")

# export summary data
write.csv(eval_df, file = "./output/evaluation/01_validation_summary.csv")
write.xlsx(eval_df, file = "./output/evaluation/01_validation_summary.xlsx", sheetName = "Sheet1", showNA = FALSE)


### Calculate average metrics for valid replicates

# filter replicates to be averaged
values_toaverage <- subset(eval_df, grepl("include", eval_df$decision), select = c("scenario","species","Replica", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI"))

# calculate mean value for evaluation metrics by species per scenario
metric_average <- aggregate(values_toaverage[,4:8], by = list(values_toaverage$species, values_toaverage$scenario), FUN = mean)
colnames(metric_average) <- c("species","scenario", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI")

# export averaged metrics
write.csv(metric_average, file = "./output/evaluation/01_validation_average.csv")
write.xlsx(metric_average, file = "./output/evaluation/01_validation_average.xlsx", sheetName = "Sheet1", showNA = FALSE)

### Averaged maps
# identify replicates output files to be averaged
replicates_toaverage <- subset(eval_df, grepl("include", eval_df$decision), select = c(scenario,species,Replica,CBI))
sc_id <- replicates_toaverage$scenario
sp_id <- replicates_toaverage$species
rep_id <- replicates_toaverage$Replica
rep_cbi <- replicates_toaverage$CBI

replicates_toaverage$ID <- gsub("species_", "", replicates_toaverage$Replica)
replicates_toaverage$ID <- as.numeric(replicates_toaverage$ID)
replicates_toaverage$ID <- replicates_toaverage$ID + 1
replicates_toaverage$ID <- paste0("_", replicates_toaverage$ID, ".tif")

# export name of output files that are going to be averaged to built final map
write.csv(replicates_toaverage, file = "./output/evaluation/rep_toaverage.csv")


############################################
### generate averaged maps per species ####
############################################

# select only scenarios that have valid replicates
spp <- as.vector(unique(replicates_toaverage$species))
sc <- as.vector(unique(replicates_toaverage$scenario))
#write.csv(scen.name, file = "./data/scenario_names_valid_sdm.csv")


for (k in 1:length(sc)) {
  
  for (j in 1:length(spp)){
    
    # path to models folder
    path <- paste0("./output/models/",sc[k], "/", spp[j], "/")
    sdm_files <- subset(replicates_toaverage,
                        grepl(spp[j], replicates_toaverage$species) & 
                          grepl (sc[k], replicates_toaverage$scenario),
                        select = ID) 
    
    sdm <- paste0(path,sdm_files$ID)
    # stack maps
    sdmStack <- rast(sdm)
    
    # averaged map
    sdm_avg <- mean(sdmStack)
    
    # averaged maps by CBI
    cbi <- subset(replicates_toaverage, grepl(spp[j], replicates_toaverage$species) & 
                    grepl (sc[k], replicates_toaverage$scenario),select = CBI)
    cbi.v <- cbi[,1]
    sdm_cbi <- terra::weighted.mean(sdmStack,cbi.v)
    
    # export averaged model map
    writeRaster(sdm_avg, filename = paste0(path,spp[j],"_avg.tif"), overwrite=TRUE)
    writeRaster(sdm_cbi, filename = paste0(path,spp[j],"_cbi.tif"), overwrite=TRUE)
  }
  
} # end of loop for scenarios

##################
####

