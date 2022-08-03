library(terra)

path_vect <- "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/shapes_out/"
path_raster <- "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/rasterized/"
path_out_temp <- "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/out_temp_ras/"

dir.create(path_raster)
dir.create(path_out_temp)

nrw <- st_read("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/nrw.shp")


env_vector_ls <- paste0(path_vect, list.files(path_vect, pattern = "*.shp$"))
env_vector <- lapply(env_vector_ls, st_read)

## rasterize
terra::terraOptions(tmpdir=path_out_temp, overwrite = TRUE)

resolution = 1000

raster_template <- rast(ext(env_vector[[1]]), resolution = resolution, 
                        crs = crs(env_vector[[1]]))

nms <- c("cultiv", "grassland", "railway", 
         "rivers", "settlement", "streets")

lc_rasterized <- mapply(function(x,y) {
  rasterize(x=x,y=raster_template,field=1,
            filename = paste0(path_raster, y, "_rast.tif"), overwrite=TRUE)
}, x = env_vector, y = nms)


lc <- rast(paste0(path_raster, list.files(path_raster)))

##

template <- stars::st_as_stars(raster_template)
ras <- st_rasterize(env_vector[[1]], template, file = paste0(path_raster, "ackr_rast.tif"),
             align = TRUE)

library(stars)
lc_rasterized <- mapply(function(x,y) {
  gdal_rasterize(x,paste0(path_raster, y, "_rast.tif"))
}, x = env_vector_ls, y = nms)




