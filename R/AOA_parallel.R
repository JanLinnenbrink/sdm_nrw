library(CAST)
library(caret)
library(terra)
library(raster)
library(sf)
library(parallel)


# this code takes the predictors (as .grd) and the final model as an input
# it splits the raster in n tiles and loops over these tiles in parallel

# specify path to files (scratch)
path_scratch <- "/" # ending with a forward slash

# load the predictors and the model
predictors <- rast(paste0(path_scratch, "predictors.grd"))
model <- readRDS(paste0(path_scratch, "model"))

# specify the number of cores
ncores <- 4

# specify the number of tiles (only square numbers, e.g. 1,4,9,16,25,36,49,64)
ntiles <- 4

### calculate the AOA in parallel ###

# calculate DI
model_trainDI = trainDI(model)
print(model_trainDI)

# get geographical extent of predictors
ug <- ext(predictors[[1]])
ug <- st_as_sf(as.polygons(ug))
st_crs(ug) <- st_crs(predictors[[1]])

# create tile polygon
ug_tiles <- st_make_grid(ug, n=sqrt(ntiles))
plot(ug_tiles)
ug_df <- data.frame(n = 1:ntiles)
ug_df$geom <- st_sfc(ug_tiles)
ug_tiles <- st_as_sf(ug_df)
ug_tiles <- vect(ug_tiles)

# partition the predictors in tiles
predictors_l <- as.list(predictors)
t1 <- list()
for (i in seq_along(predictors_l)) {
  t1[[i]] <- terra::crop(predictors_l[[i]], ug_tiles)
}

ug_tiles_l <- terra::split(ug_tiles, "n")
preds_tiled <- lapply(ug_tiles_l, function(x){
  lapply(t1, terra::crop, y=x)
})

preds_tiled <- mclapply(preds_tiled, function(x) raster::stack(lapply(x,raster)),
                        mc.cores = ncores)

# calculate the AOA
tiles_aoa <- mclapply(preds_tiled, function(tile){
  aoa(newdata = tile, trainDI = model_trainDI)
}, mc.cores = ncores)

# merge the AOA
aoa_rasterl <- lapply(tiles_aoa, function(x){rast(x$AOA)})
aoa <- do.call(terra::merge,aoa_rasterl)

# merge the DI
di_rasterl <- lapply(tiles_aoa, function(x){rast(x$DI)})
di <- do.call(terra::merge,di_rasterl)

# export AOA and DI
writeRaster(aoa, paste0(path_scratch, "aoa.tif"))
writeRaster(di, paste0(path_scratch, "di.tif"))
