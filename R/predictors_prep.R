require(sf)
require(fasterize)
require(terra)
require(raster)

vc2ras <- function(path, resolution, fields) {
  env_vector_ls <- paste0(path, "/", list.files(path, pattern = "*.shp"))
  env_vector <- lapply(env_vector_ls, st_read)
  
  raster_template <- rast(ext(env_vector[[1]]), resolution = 10, 
                          crs = st_crs(env_vector[[1]])$wkt)
  
  message("converting to spatvec")
  
  env_spatvec <- lapply(env_vector, terra::vect)
  
  message("converting to raster")
  
  ras_ls <- list()
  for(i in seq_along(env_vector)) {ras_ls[[i]]=terra::rasterize(env_spatvec[[i]],raster_template,field=fields[[i]])}
  
  message("stacking")
  
  return(stack(lapply(ras_ls, raster)))
}


# change path to the folder with your shapefiles
path <- "C:/0_Msc_Loek/M7_Fernerkundung/shapes"

# change fields according to the relevant fields in your shape files
fields <- list("BAUMART", "ZIEL", "HECKENART", "NUTZUNG","OBJECTID")

# resolution in meters
resolution <- 10

env_data_vc <- vc2ras(path=path, resolution = resolution, fields = fields)

ndom <- raster("C:/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/ndom_5.tif")
dgm <- raster("C:/Users/janli/sciebo/FE_22_Citizen_Science/data/environmental_data/dgm_5.tif")

env_data <- stack(env_data_vc[[1]], env_data_vc[[n]], ndom, dgm)
names(env_data) <- c("d", "d", "ndom", "dgm")

writeRaster(env_data, "env_data.tif")
