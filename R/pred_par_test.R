.libPaths("C:/R/R_library/")

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

rasterOptions(tmpdir=path_out_temp, overwrite=TRUE)
terraOptions(tempdir=path_out_temp, overwrite=TRUE)

# load the predictors and the model
predictors <- rast("e:/sdm_nrw/predictors_sel.tif") |> 
  terra::crop(y=vect("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/extent_test.shp"))

model <- readRDS(paste0(path_in, "model"))


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
m <- c(0, 0.01, 0, 0.01, 1, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
preds_tiled <- lapply(preds_tiled, function(x) {
  forest <- classify(x$forest, rclmat, include.lowest=TRUE)
  forest <- as.factor(forest)
  cultiv <- classify(x$cultiv, rclmat, include.lowest=TRUE)
  cultiv <- as.factor(cultiv)
  levels(forest) <- c("no_forest", "forest")
  levels(cultiv) <- c("no_cultiv", "cultiv")
  ndvi <- x$ndvi
  ndom_5 <- x$ndom_5
  dist_settl <- x$dist_settl
  rs_l <- list(ndom_5, cultiv, dist_settl, forest, ndvi)
  retrn <- rast(rs_l)
  return(retrn)
})

for(i in seq_along(preds_tiled)) {
  terra::writeRaster(preds_tiled[[i]],paste0(path_out_temp, i, "_rast.tif"),
                     overwrite = TRUE)
}


### prediction using terra ###
prediction <- lapply(preds_tiled, function(x) {
  predict(x, model=model)
} )

writeRaster(prediction$n, paste0(path_out, "prediction.tif"))

rm(preds_tiled); rm(prediction)

### calculate the AOA in parallel ###

# load data in raster format (not terra)
rasterf <- list.files(path_out_temp, pattern = "*_rast.tif$",
                      full.names = TRUE)

preds_stack <- lapply(rasterf, stack)

preds_stack <- lapply(preds_stack, function(x) {
  forest <- reclassify(x$forest, c(0,0.1,0,0.1,1,1))
  forest <- as.factor(forest)
  cultiv <- reclassify(x$cultiv, c(0,0.01,1,0.01,1,1))
  cultiv <- as.factor(cultiv)
  levels(forest) <- data.frame(ID=c(0,1), lvl= c("no_forest", "forest"))
  levels(cultiv) <- data.frame(ID=c(0,1), lvl= c("no_cultiv", "cultiv"))
  ndvi <- x$ndvi
  ndom_5 <- x$ndom_5
  dist_settl <- x$dist_settl
  preds_stack[[i]] <- stack(ndom_5, cultiv, dist_settl, forest, ndvi)
})
library(terra)
test <- rast("c:/0_Msc_Loek/Z_Palma/results/test/prediction.tif")
# calculate DI
model_trainDI = trainDI(model)
writeRaster(di, paste0(path_out, "di.tif"))

# calculate the AOA
tiles_aoa <- lapply(preds_stack, function(tile){
  aoa(newdata = tile, trainDI = model_trainDI)
})

# merge the AOA
aoa_rasterl <- lapply(tiles_aoa, function(x){rast(x$AOA)})
aoa <- do.call(terra::merge, aoa_rasterl)

# merge the DI
di_rasterl <- lapply(tiles_aoa, function(x){rast(x$DI)})
di <- do.call(terra::merge, di_rasterl)

writeRaster(aoa, paste0(path_out, "aoa.tif"))