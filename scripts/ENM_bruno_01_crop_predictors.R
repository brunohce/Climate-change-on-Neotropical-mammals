# ENM ms bruno
# by Ana Carolina Loss
# 10 AGO 2023

library(terra)

#cal <- vect("./data/shapes/calibra.shp", crs = "EPSG:4326")
#bb <- vect("./data/shapes/calibra_bb.shp", crs = "EPSG:4326")


#####################
## 1. STUDY AREA   ##
#####################

study_area <- vect("./data/shapes/calibra_bb.shp", crs = "EPSG:4326")
#study_area<-project(study_area, "EPSG:4326") #WGS84
plot(study_area)
bb <- study_area

## use extent of cliped example to create a bounding box to clip the predictors
# OBS: clipping is way faster than masking. Clipping everything before masking will speed up the process

###*** load example file to test ***###
#ex <- rast("./data/Chelsa/CHELSA_bio10_02.tif")
r <- rast("./data/wc21/miroc6_2_5/wc2.1_2.5m_bioc_MIROC6_ssp126_2061-2080.tif")
ex <- r[[1]]
#ex<-project(ex, "EPSG:4326") #WGS84
plot(ex)

# crop example file to see how it look likes
ex_bb <- crop(ex,bb)
plot(ex_bb)


##########################
##   2. PREDICTORS      ##
##########################

## Files from PRESENT
### load World Clim data
#bio_folder = "./data/Chelsa/"
bio_folder = "./data/wc21/historical/wc2.1_2.5m_bio/"

bio_path <-list.files(path = bio_folder, patter='*.tif$')
bio_files <- paste(bio_folder,bio_path, sep="")

bio <- rast(bio_files)
#bio <- project(bio, "EPSG:4326")

#### crop predictors using bb extent
bio_bb <- crop(bio,bb)
#plot(bio_bb[[3]])

#### save as .ascii in the new folder
writeRaster(bio_bb, paste0("./output/predictors/wc21/current/",names(bio_bb),".asc"))



### Files from FUTURE

# ssp1-2.6
bio <- rast("./data/wc21/miroc6_2_5/wc2.1_2.5m_bioc_MIROC6_ssp126_2061-2080.tif")
bio_bb <- crop(bio,bb)
writeRaster(bio_bb, paste0("./output/predictors/wc21/ssp126/",names(bio_bb),".asc"))

# ssp2-4.5
bio <- rast("./data/wc21/miroc6_2_5/wc2.1_2.5m_bioc_MIROC6_ssp245_2061-2080.tif")
bio_bb <- crop(bio,bb)
writeRaster(bio_bb, paste0("./output/predictors/wc21/ssp245/",names(bio_bb),".asc"))

# ssp3-7.0
bio <- rast("./data/wc21/miroc6_2_5/wc2.1_2.5m_bioc_MIROC6_ssp370_2061-2080.tif")
bio_bb <- crop(bio,bb)
writeRaster(bio_bb, paste0("./output/predictors/wc21/ssp370/",names(bio_bb),".asc"))

# ssp5-8.5
bio <- rast("./data/wc21/miroc6_2_5/wc2.1_2.5m_bioc_MIROC6_ssp585_2061-2080.tif")
bio_bb <- crop(bio,bb)
writeRaster(bio_bb, paste0("./output/predictors/wc21/ssp585/",names(bio_bb),".asc"))

################
########
##

