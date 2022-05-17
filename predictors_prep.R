require(stars)
require(sf)

vc2ras <- function(path) {
  env_vector_ls <- list.files(path, pattern = "*.shp")
  env_vector <- lapply(env_vector_ls, st_read)
  env_raster <- stack(lapply(env_vector, st_rasterize))
}


