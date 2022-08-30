.libPaths('/home/j/jlinnenb/r_packages/')

library(terra)
library(sf)
library(CAST, lib.loc = "/home/j/jlinnenb/r_packages/")
library(caret)
library(mlr3)
library(parallel)
library(raster)


# specify path to files (scratch)
path_in <- "/scratch/tmp/jlinnenb/sdm_nrw/model_results/"
path_out_temp <- "/scratch/tmp/jlinnenb/sdm_nrw/out_temp/"
path_out <- "/scratch/tmp/jlinnenb/sdm_nrw/aoa/"

rasterOptions(tmpdir=path_out_temp, overwrite=TRUE)
terraOptions(tempdir=path_out_temp, overwrite=TRUE)


## load data ##

lrn <- readRDS(paste0(path_in, "lrn"))
task <-readRDS(paste0(path_in, "task"))
folds <- readRDS(paste0(path_in, "folds"))
tr <- readRDS(paste0(path_in, "tr"))
model_trainDI <- readRDS(paste0(path_in, "model_trainDI"))
predictors_sample <- rast(paste0(path_in, "predictors_sel.tif"))|> 
  aggregate(4)


## split predictors in tiles ##

ntiles = ncores = 16

tile_preds <- function(x) {
  # get geographical extent of predictors
  ug <- st_bbox(x[[1]])
  ug <- st_as_sf(st_as_sfc(ug))
  
  # create tile polygon
  ug_tiles <- st_make_grid(ug, n=sqrt(ntiles))
  ug_df <- data.frame(n = 1:ntiles)
  ug_df$geom <- st_sfc(ug_tiles)
  ug_tiles <- st_as_sf(ug_df)
  
  # partition the predictors in tiles
  preds_tiled <- list()
  for (i in c(1:nrow(ug_tiles))) {
    preds_tiled[[i]] <- terra::crop(x=x, y=ug_tiles[i,])
  }
  return(preds_tiled)
}

recl <- function(x) {
  # reclassify forest and cultivated rasters
  m <- c(0, 0.01, 0, 0.01, 10^20, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  x$cultiv <- terra::classify(x$cultiv, rclmat)
  
  m <- c(0, 0.01, 0, 0.01, 10^20, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  x$forest <- terra::classify(x$forest, rclmat)
  return(x)
}

predictors_tiled <- tile_preds(predictors_sample) |> 
  lapply(recl)


## calculate AOA ##

AOA <- mclapply(predictors_tiled, function(x){
  aoa(newdata =  raster::stack(x),
      train = tr[,-1],
      variables = task$feature_names,
      weight = data.frame(t(lrn$importance())),
      CVtest = folds,
      trainDI = model_trainDI)
  
}, mc.cores = ncores)

print(AOA)

AOA_AOA <- lapply(AOA, function(x) x$AOA) 
AOA_AOA <- do.call(merge, AOA_AOA)

AOA_DI <- lapply(AOA, function(x) x$DI) 
AOA_DI <- do.call(merge, AOA_DI)

writeRaster(AOA_AOA, paste0(path_out, "AOA_test.tif"), overwrite=TRUE)
writeRaster(AOA_DI, paste0(path_out, "DI_test.tif"), overwrite=TRUE)

