# ENM ms bruno
# by Ana Carolina Loss
# 15 AGO 2023

### Richness from binary distribution raster


library(terra)


# upload raster to use as model
mask <- rast("./output/predictors/wc21/current/wc2.1_2.5m_bio_1.asc")
mask <- project(mask, "EPSG:4326")

# upload calibration area shape file (cerrado + atlantic forest)
cal <- vect("./data/shapes/calibra.shp", crs = "EPSG:4326")
# rasterize lei shape file to study area extent and resolution
cal.r <- rasterize(cal, mask, background = 0, touches = TRUE)

# scenario names
sc <- list.files("./output/binary_maps/", full.names = FALSE)

for (k in 1:length(sc)) {
  
  # species with binary distribution maps
  spp.name <- list.files(paste0("./output/binary_maps/", sc[k], "/"), patter = '*_dist_1NA.tif$', full.names = FALSE)
  
  # make raster to store species distribution binary raster (1 for presence / 0 background)
  r <- mask
  
  for (i in 1:length(spp.name)){
    dist.path <- paste0("./output/binary_maps/", sc[k],"/", spp.name[i])
    bin <- rast(dist.path)
    
    # resample raster to lower resolution
    bin <- resample(bin,cal.r, method="near")
    r <- c(r,bin)
    
  }
  
  r <- r[[-1]]
  all <- sum(r, na.rm = TRUE)
  writeRaster(all, filename= paste0("./output/richness/", sc[k], ".tiff"), overwrite=T)
  
  
  all.cal <- all*cal.r
  writeRaster(all.cal, filename= paste0("./output/richness/", sc[k], "_cal.tiff") , overwrite=T)
  
  maskNA <- all.cal/all.cal
  all.cal.mask <- all.cal * maskNA
  writeRaster(all.cal.mask, filename= paste0("./output/richness/", sc[k], "_cal1NA.tiff"), overwrite=T)
  
  
} # end looop for scenarios




###################
###########
##
