require(sf)
require(terra)
require(raster)
library(future)
library(rgdal)
library(parallel)
library(dplyr)

setwd("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw")

vc2ras <- function(path, resolution, fields) {
  env_vector_ls <- paste0(path, "/", list.files(path, pattern = "*.shp"))
  env_vector <- lapply(env_vector_ls, st_read)
  
  raster_template <- rast(ext(env_vector[[1]]), resolution = resolution, 
                          crs = st_crs(env_vector[[1]])$wkt)
  
  message("converting to spatvec")
  
  env_spatvec <- lapply(env_vector, terra::vect)
  
  message("converting to raster")
  
  ras_ls <- list()
  for(i in seq_along(env_vector)) {ras_ls[[i]]=terra::rasterize(env_spatvec[[i]],raster_template,field=fields[[i]])}
  
  message("stacking")
  
  return(stack(lapply(ras_ls, raster)))
}

env_vector_ls <- paste0(getwd(),"/shapes/", list.files(paste0(getwd(),"/shapes/"), pattern = "*.shp"))
env_vector <- lapply(env_vector_ls, st_read)
env_vector <- env_vector[c(1,5,6)]

env_spatvec <- lapply(env_vector, terra::vect)
env_spatvec[[1]]$id <- 1:nrow(env_spatvec[[1]])
env_spatvec[[2]]$id <- 1:nrow(env_spatvec[[2]])
env_spatvec[[3]]$id <- 1:nrow(env_spatvec[[3]])

raster_template <- raster(extent(env_vector[[1]]), resolution = 5, 
                        crs = st_crs(env_vector[[1]])$wkt)




############ lower resolution -> good output

#
env_vector_ls <- paste0(getwd(),"/shapes/", list.files(paste0(getwd(),"/shapes/"), pattern = "*.shp"))

rivers_try <- st_read(env_vector_ls[[1]]) 

raster_template <- raster(extent(rivers_try), resolution = 500, 
                          crs = st_crs(rivers_try$wkt))

rivers_try_r <- terra::rasterize(rivers_try,raster_template,field="Id")
rivers_dist_try <- terra::distance(rivers_try_r)
rivers_dist_crop <- terra::mask(rivers_dist_try,nrw)


#
############

rivers <- st_read(env_vector_ls[[1]])

raster_template <- raster(extent(rivers_try), resolution = 5, 
                          crs = st_crs(rivers_try$wkt))

rivers_r <- terra::rasterize(rivers_try,raster_template,field="Id")
rivers_dist <- terra::distance(rivers_try_r)
rivers_dist_crop <- terra::mask(rivers_dist_try,nrw)



# grassland
rivers <- st_read(env_vector_ls[[1]])

raster_template <- raster(extent(rivers_try), resolution = 5, 
                          crs = st_crs(rivers_try$wkt))

rivers_r <- terra::rasterize(rivers_try,raster_template,field="Id")
rivers_dist <- terra::distance(rivers_try_r)
rivers_dist_crop <- terra::mask(rivers_dist_try,nrw)



###### higher resoultion

ras_ls <- list()
for(i in seq_along(env_vector)) {ras_ls[[i]]=terra::rasterize(env_vector[[i]],raster_template,field="id")}

urban <- ras_ls[[2]]
m <- c(0,0,0, 0.1,999999999,1)
rm <- matrix(m, ncol=3, byrow=TRUE)

ras_ls[[2]] <- terra::classify(ras_ls[[2]], rm, include.lowest=TRUE)
ras_ls[[3]] <- terra::classify(ras_ls[[3]], rm, include.lowest=TRUE)




###

names <- c("rivers", "urban", "streets")




####################################################################
rasterf <- list.files(pattern="*.tif")
rsu <- rast(rasterf)

germany <- readRDS("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/R/gadm36_DEU_2_sp.rds")
nrw <- germany[germany$NAME_1 == "Nordrhein-Westfalen",]%>% 
  st_as_sf() %>% 
  st_transform(st_crs(25832))

nrw_bb <- st_as_sf(st_as_sfc(st_bbox(nrw)))

# create tile polygon
ntiles <- 9
ug_tiles <- st_make_grid(nrw_bb, n=sqrt(ntiles))
ug_df <- data.frame(n = 1:ntiles)
ug_df$geom <- st_sfc(ug_tiles)
ug_tiles <- st_as_sf(ug_df)
ug_tiles <- vect(ug_tiles)

# partition the predictors in tiles
ug_tiles_l <- terra::split(ug_tiles, "n")
preds_tiled <- lapply(ug_tiles_l, function(x) terra::crop(x=rsu, y=x))
preds_tiled <- lapply(preds_tiled, stack)

# distance calculation

rasterOptions(tmpdir="/media/sf_C_DRIVE/r_temp")
as.list(preds_tiled[[1]])
dist_ras <- mclapply(preds_tiled, function(x){lapply(as.list(x), function(y) {raster::distance(y)})},
                     mc.cores = 3)
              
#
############
dummy <- terra::rast(x=ext(nrw_bb),resolution=5,crs=crs(25832),vals=1)
rivers <- vect(env_vector_ls[[1]])


dist_rivers <- terra::distance(rivers, dummy)
              
stopCluster(cl)

#####################################################################







system("gdal_proximity.py rivers.tif rivers_Copy.tif") 

foreach (i=1:3) %dopar% {
  dist[i] <- terra::distance(rsu[[i]])
}

dist_ras <- lapply(as.list(rsu), raster::distance)

dist_ras <-lapply(as.list(rsu),raster::distance)

dist1 <- distance(rsu[[1]])

dist_ras <- lapply(as.list(rsu), function(x) terra::distance(x))

mapply(writeRaster, dist_ras, paste0(path,c("rivers_dist","streets_dist","urban_dist"), ".tif"))


dist_ras <- lapply(ras_ls, terra::distance)

mapply(function(x,y) writeRaster(x, paste0(y,".tif")), x=ras_ls,y=names)








# change path to the folder with your shapefiles
path <- "/media/sf_C_DRIVE/0_Msc_Loek/M7_Fernerkundung/shapes/"

# change fields according to the relevant fields in your shape files
fields <- list("VEG", "Id", "Id", "OBJART_TXT","OBJART", "VEG")

# resolution in meters
resolution <- 5

env_data_vc <- vc2ras(path=path, resolution = resolution, fields = fields)

names <- c("acker", "bahn", "gewaesser", "siedlungen", "strassen", "wald")

names(env_data_vc) <- names


ndom <- raster("/media/sf_C_DRIVE/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/ndom_5.tif")
dgm <- raster("/media/sf_C_DRIVE/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/dgm_5.tif")
aspect <- raster("/media/sf_C_DRIVE/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/aspect_5.tif")
slope <- raster("/media/sf_C_DRIVE/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/slope_5.tif")

lr <- list(ndom, dgm, aspect, slope)
lrs <- lapply(lr,resample,env_data_vc)

env_data <- stack(env_data_vc[[1]], env_data_vc[[2]],env_data_vc[[3]],env_data_vc[[4]],
                  env_data_vc[[5]],env_data_vc[[6]], lrs[[1]], lrs[[2]], lrs[[3]], lrs[[4]])

rivers <- env_data$gewaesser
streets <- env_data$strassen
urban <- env_data$siedlungen

dist_rasl <- list(rivers, streets, urban)
mapply(writeRaster, dist_rasl, paste0(path,c("rivers","streets","urban"), ".tif"))

writeRaster(env_data, "env_data.grd", format = "raster")


env_data <- rast("D:/env_data/env_data.grd")

plot(rivers)




env_data <- rast("e:/sdm_nrw/env_data.tif")
new_names <- c("acker", "bahn_5", "rivers_dist", "settlements_dist", "streets_dist")
env_data <- env_data[[!names(env_data) %in% new_names]]

dist <- rast(list.files("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/eucl_distance/",
                        pattern="*.tif$", full.names = TRUE))

other_ras <- rast(list.files("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/out_raster/",
                             pattern="*.tif$", full.names = TRUE))

path_res <- "e:/resampled/"
dir.create(path_res)

terraOptions(tempdir = "e:/r_temp")


dist_res <- lapply(dist[[3:6]], function(x) {
  terra::resample(x, env_data[[1]], filename = paste0(path_res,names(x), ".tif"))
})

other_ras <- lapply(other_ras[[1:2]], function(x){
  x[!is.na(x)] <- 1
  x[is.na(x)] <- 0
  return(x)
})


other_res <- lapply(other_ras, function(x) {
  terra::resample(x, env_data[[1]], filename = paste0(path_res,names(x), ".tif"),
                  overwrite=TRUE)
})


lapply(other_res, function(x){
  writeRaster(x, paste0(path_res,names(x), ".tif"))
})

new <- rast(list.files("e:/resampled", full.names=TRUE))

env_data <- rast(list(env_data, new))

writeRaster(env_data, "e:/predictors.grd")






### ab hier 
predictors <-  rast("e:/predictors.tif")


ndvi <- rast("E:/sdm_nrw/resampled/ndvi_f_res.tif")
aspect <- rast("E:/sdm_nrw/resampled/aspect_5_res.tif")
focal_ndvi <- rast("E:/sdm_nrw/resampled/focal_sd_res.tif")
dgm <- rast("E:/sdm_nrw/resampled/dgm_5_res.tif")
slope <- rast("E:/sdm_nrw/resampled/slope_5_res.tif")
ndom_5 <- rast("E:/sdm_nrw/resampled/ndom_5_res.tif")

dist_river <- rast("e:/resampled/rivers_distance.tif")
dist_settl <- rast("e:/resampled/settlement_distance.tif")
dist_street <- rast("e:/resampled/streets_distance.tif")

forest <- predictors[["wald"]]
cultiv <- predictors[["cultiv"]]
cls <- data.frame(id=0:1, cover=c("no_cultiv", "cultiv"))
levels(cultiv) <- cls
grass <- predictors[["grassland"]]
cls <- data.frame(id=0:1, cover=c("no_grassland", "grassland"))
levels(grass) <- cls

preds_new <- rast(list(ndvi, aspect, focal_ndvi, dgm, slope, ndom_5,
                     dist_river, dist_settl, dist_street,
                     forest, cultiv, grass))
names(preds_new) <- c("ndvi", "aspect", "focal_ndvi", "dgm", "slope", "ndom_5",
                           "dist_river", "dist_settl", "dist_street",
                           "forest", "cultiv", "grass")               
          
predictors <- preds_new       
rm(preds_new)
writeRaster(predictors, "e:/sdm_nrw/predictors.tif", overwrite=TRUE)


