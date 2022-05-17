require(sf)
require(fasterize)
library(terra)


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

env_data <- vc2ras(path=path, resolution = resolution, fields = fields)

