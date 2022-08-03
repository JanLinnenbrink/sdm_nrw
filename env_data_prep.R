library(sf)
library(raster)
library(ggplot2)
library(terra)
library(dplyr)


setwd("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/dlm50_EPSG25832_Shape")


nrw <- germany[germany$NAME_1 == "Nordrhein-Westfalen",]%>% 
  st_transform(st_crs(25832))
nrw <- st_union(nrw)
nrw_spat <- terra::vect(nrw)
st_write(nrw, "C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw.shp")

nrw <- st_read("C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw.shp")
nrw <- vect(nrw)

env_data <- rast("env_data_aktuell1.tif")


# extract grassland from cultivated land

acker <- read_sf("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/dlm50_EPSG25832_Shape/veg01_f.shp")

ackerland <- acker[acker$VEG == 1010,] 
gruenland <- acker[acker$VEG == 1020,] 

raster_template <- env_data$acker
values(raster_template) <- 1

acker_ras <- rasterize(vect(st_geometry(ackerland)), raster_template, field = 1, background = 0)
gruenland_ras <- rasterize(vect(st_geometry(gruenland)), raster_template, field = 1, background = 0)

acker_masked <- terra::mask(acker_ras, nrw)
gruenland_masked <- terra::mask(gruenland_ras, nrw)

terra::writeRaster(acker_masked, "acker_new.tif", overwrite=TRUE)
terra::writeRaster(gruenland_masked, "gruenland.tif", overwrite=TRUE)


env_data$acker <- acker_masked
env_data$gruenland <- gruenland_masked

terra::writeRaster(env_data, "env_data_updated.tif", overwrite=TRUE)



# load env data & calculate focal statistics
#setwd("D:/env_data/resampled_old") # eigentlich ohne letzten folder
setwd("D:/env_data/") 

files <- list.files(pattern = "*.tif$")


#remove <- c("env_data.tif", "env_data_masked.tif", "env_data_sr.tif",
#            "env_pa.tif", "masked_env_data.tif", "rlayer.tif", "slope_5_res.tif")
#files <- files[! files %in% remove]

rasters <- lapply(files, rast)

#rasters$ndvi_sd <- terra::focal(rasters$ndvi_f.tif, fun = "sd", filename ="ndvi_sd.tif")
#rasters$ndvi_sd <- terra::crop(rasters$ndvi_sd, nrw_spat)

dir.create("resampled")

raster_res <- list()
for (i in seq_along(rasters)) {
  raster_res[[i]] <- terra::resample(x=rasters[[i]], y=rasters[[1]],
                                     filename=paste0("resampled/",names(rasters[[i]]), "_res.tif"),
                                     overwrite=TRUE)
}

rasterfiles <- list.files("resampled/")
env_data <- terra::rast(rasterfiles)

terraOptions(tempdir = "D:/env_data/resampled_old")
env_data <- terra::mask(env_data, nrw)

terra::writeRaster(env_data, "env_data_masked1.tif")


# reclassify binary data (necessary due to resampling)
m <- c(0,0,0, 0.1,255,1)
rm <- matrix(m, ncol=3, byrow=TRUE)

env_data$acker <- terra::classify(env_data$acker, rm, include.lowest=TRUE)
env_data$bahn_5 <- terra::classify(env_data$bahn_5, rm, include.lowest=TRUE)
env_data$wald <- terra::classify(env_data$wald, rm, include.lowest=TRUE)

terra::writeRaster(env_data, "env_data_aktuell1.tif")

env_data <- rast("env_data_aktuell1.tif")

env_data$acker <- as.factor(env_data$acker)
levels(env_data$acker) <- c("no_cultiv", "cultiv")
env_data$bahn_5 <- as.factor(env_data$bahn_5)
levels(env_data$bahn_5) <- c("no_railw", "railw")
env_data$wald <- as.factor(env_data$wald)
levels(env_data$wald) <- c("no_forest", "forest")


terra::writeRaster(env_data, "D:/env_data.tif", overwrite=TRUE)


