library(terra)
library(sf)
library(stars)
library(parallel)

path_vect <- "/media/sf_C_DRIVE/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/shapes_out/"
path_raster <- "/media/sf_C_DRIVE/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/rasterized/"
path_out_temp <- "/media/sf_C_DRIVE/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/out_temp_ras/"

dir.create(path_raster)
dir.create(path_out_temp)

nrw <- st_read("/media/sf_C_DRIVE/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/nrw.shp")

env_vector_ls <- paste0(path_vect, list.files(path_vect, pattern = "*.shp$"))
env_vector <- lapply(env_vector_ls, st_read)

## rasterize
terra::terraOptions(tmpdir=path_out_temp, overwrite = TRUE)

resolution = 5

raster_template <- st_as_stars(rast(ext(env_vector[[1]]), resolution = resolution, 
                        crs = crs(env_vector[[1]])))

nms <- c("cultiv", "grassland", "railway", 
         "rivers", "settlement", "streets")

lc_rasterized <- mcmapply(function(x,y) {
  write_stars(st_rasterize(x, raster_template, file = paste0(path_out_temp, y, "_rast.tif")),
              paste0(path_raster, y, "_rast.tif"))
}, x = env_vector, y = nms, mc.cores = 6)

