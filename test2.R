.libPaths('/home/j/jlinnenb/r_packages/')

library(CAST)
library(caret)
library(terra)
library(raster)
library(sf)
library(parallel)


# this code takes the predictors and the final model as an input
# it splits the raster in n tiles and loops over these tiles in parallel

# specify path to files (scratch)
path_in <- "e:/sdm_nrw/model_results/"
path_out_temp <- "e:/sdm_nrw/out_temp/"
path_out <- path_out_temp

dir.create(path_out)

rasterOptions(tmpdir=path_out_temp)
terraOptions(tempdir=path_out_temp)

# load the predictors and the model
predictors <- rast("e:/sdm_nrw/predictors_sel.tif") |> 
  terra::crop(y=vect("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/extent_test.shp"))

model <- readRDS(paste0(path_in, "ffs_model"))


# specify the number of tiles (only square numbers, e.g. 1,4,9,16,25,36,49,64)
ntiles <- 9

# specify the number of cores
ncores <- ntiles

### partition the predictor raster ###

# get geographical extent of predictors
ug <- ext(predictors[[1]])
ug <- st_as_sf(as.polygons(ug))
st_crs(ug) <- st_crs(predictors[[1]])

# create tile polygon
ug_tiles <- st_make_grid(ug, n=sqrt(ntiles))
ug_df <- data.frame(n = 1:ntiles)
ug_df$geom <- st_sfc(ug_tiles)
ug_tiles <- st_as_sf(ug_df)
ug_tiles <- vect(ug_tiles)

# partition the predictors in tiles
ug_tiles_l <- terra::split(ug_tiles, "n")
preds_tiled <- lapply(ug_tiles_l, terra::crop, x=predictors)

preds_tiled <- lapply(rasterf, raster::stack)


### predict in parallel ###
prediction <- lapply(preds_tiled, function(x) {
  raster::predict(x, model=model$finalModel,
                  factors=list(forest=levels(x$forest),
                               cultiv=levels(x$cultiv)),
                  type = "prob")
} )

# merge the prediction
prediction_rasterl <- lapply(prediction, function(x){rast(x)})
prediction <- do.call(terra::merge, prediction_rasterl)


### calculate the AOA in parallel ###

# calculate DI
model_trainDI = trainDI(model)

# calculate the AOA
tiles_aoa <- mclapply(preds_tiled, function(tile){
  aoa(newdata = tile, trainDI = model_trainDI)
}, mc.cores = ncores)

# merge the AOA
aoa_rasterl <- lapply(tiles_aoa, function(x){rast(x$AOA)})
aoa <- do.call(terra::merge, aoa_rasterl)

# merge the DI
di_rasterl <- lapply(tiles_aoa, function(x){rast(x$DI)})
di <- do.call(terra::merge, di_rasterl)


### export the AOA, DI and prediction
writeRaster(aoa, paste0(path_out, "aoa.tif"))
writeRaster(di, paste0(path_out, "di.tif"))
writeRaster(prediction, paste0(path_out, "prediction.tif"))


