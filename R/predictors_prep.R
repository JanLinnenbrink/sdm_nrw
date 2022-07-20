require(sf)
require(fasterize)
require(terra)
require(raster)
library(future)
library(rgdal)

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

env_vector_ls <- paste0(path, "/", list.files(path, pattern = "*.shp"))
env_vector <- lapply(env_vector_ls, st_read)
env_vector <- env_vector[c(1,5,6)]

env_spatvec <- lapply(env_vector, terra::vect)
env_spatvec[[1]]$id <- 1:nrow(env_spatvec[[1]])
env_spatvec[[2]]$id <- 1:nrow(env_spatvec[[2]])
env_spatvec[[3]]$id <- 1:nrow(env_spatvec[[3]])

raster_template <- rast(ext(env_vector[[1]]), resolution = 5, 
                        crs = st_crs(env_vector[[1]])$wkt)

ras_ls <- list()
for(i in seq_along(env_vector)) {ras_ls[[i]]=terra::rasterize(env_spatvec[[i]],raster_template,field="id")}

urban <- ras_ls[[2]]
m <- c(0,0,0, 0.1,999999999,1)
rm <- matrix(m, ncol=3, byrow=TRUE)

ras_ls[[2]] <- terra::classify(ras_ls[[2]], rm, include.lowest=TRUE)
ras_ls[[3]] <- terra::classify(ras_ls[[3]], rm, include.lowest=TRUE)


names <- c("rivers", "urban", "streets")

rasterf <- list.files(pattern="*.tif")

for (raster in rasterf) {
  gdalDistance
}

rsu <- stack(rasterf)

plan(multisession, workers=3)

dist_ras <- lapply(as.list(rsu), raster::distance)

dist_ras <-lapply(as.list(rsu),raster::distance)

dist1 <- distance(rsu[[1]])

dist_ras <- lapply(as.list(rsu), function(x) terra::distance(x))

mapply(writeRaster, dist_ras, paste0(path,c("rivers_dist","streets_dist","urban_dist"), ".tif"))


dist_ras <- lapply(ras_ls, terra::distance)

mapply(function(x,y) writeRaster(x, paste0(y,".tif")), x=ras_ls,y=names)








# change path to the folder with your shapefiles
path <- "C:/0_Msc_Loek/M7_Fernerkundung/shapes/"

# change fields according to the relevant fields in your shape files
fields <- list("VEG", "Id", "Id", "OBJART_TXT","OBJART", "VEG")

# resolution in meters
resolution <- 5

env_data_vc <- vc2ras(path=path, resolution = resolution, fields = fields)

names <- c("acker", "bahn", "gewaesser", "siedlungen", "strassen", "wald")

names(env_data_vc) <- names


ndom <- raster("C:/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/ndom_5.tif")
dgm <- raster("C:/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/dgm_5.tif")
aspect <- raster("C:/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/aspect_5.tif")
slope <- raster("C:/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/slope_5.tif")

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
