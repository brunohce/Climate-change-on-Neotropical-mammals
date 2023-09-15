# ENM ms bruno
# by Ana Carolina Loss
# 11 AGO 2023

## mask raster predictors for building the models (calibration area) using mcp shape for each species

library(terra)

# upload predictors

### load WorldClim data from cropped in script 01
bio_folder = "./output/predictors/wc21/current/"
bio_path <-list.files(path = bio_folder, patter='*.asc$')
bio_files <- paste(bio_folder,bio_path, sep="")
bio <- rast(bio_files)

# upload species occurrence file
pts<- read.csv("./data/TableS1_SpeciesRecords.csv")
pts$sp <- gsub(" ", "_", pts$sp)

# create shape file from pts
xy <- vect(pts, crs = "EPSG:4326", geom = c("lon", "lat"))

# make mcp for each species
mcp <- convHull(xy, "sp")

# buffer mcp 200 km
b <-buffer(mcp, 2000, quadsegs=10)

# loop to crop predictors
 for (i in 1:length(b)){
   v <- b[i]
   c <- crop(bio,v)
   m <- mask(c,v)
   id <- v$sp
   out.dir <- paste0("./output/predictors/", id, "/")
   dir.create(out.dir)
   writeRaster(m, paste0(out.dir,names(m),".asc"))
 }

#########################
############
###
