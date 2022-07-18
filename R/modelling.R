library(caret)
library(CAST)
library(NNDM)
library(terra)
library(sf)
library(dplyr)
library(purrr)


library("NNDM")
library("caret")
library("sp")
library("sf")
library("knitr")
library("gstat")
library("gridExtra")

env_data <- rast("e:/sdm_nrw/env_data.tif")

occs <- st_read("occurences.gpkg") %>% st_transform(st_crs(env_data[[1]]))
train_dat <- st_read("train_dat.gpkg")

#occs_prep(occurences_sf = occs, env_data = env_data, 
#          path_out = "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/occurence_data")

#train_dat <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_data_new.rds")
train_dat <- train_dat[complete.cases(train_dat),]

preds <- names(train_dat[!names(train_dat) %in% c("type", "id")])


bg <- train_dat[train_dat$type == "background",]
occ <- train_dat[train_dat$type == "presence",]


bg_sample <- bg[sample(nrow(bg), nrow(occ)),]

train_dat <- rbind(bg_sample, occ)

# no occurences at railways
train_dat$bahn_5 <- NULL

train_subset <- train_dat[createDataPartition(train_dat$type,p=0.1)$Resample1,]
featurePlot(train_subset[,c("ndvi_f","dgm_5","focal_sd","slope_5")],
            factor(extr_subset$Label),plot="pairs",
            auto.key = list(columns = 2))

# train model

library(future)

library(doParallel)
cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)
stopCluster(cl)


#######

# Prediction grid
meuse.grid <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992, remove = F)

env_grid <- terra::as.points(env_data)


# Fit model
trainControl_LOO <- trainControl(method = "LOOCV", savePredictions = T)
paramGrid <-  data.frame(mtry = 2)
mod_LOO <- train(type ~ acker + aspect_5 + dgm_5 + focal_sd + ndom_5 + 
                   ndvi_f + rivers_dist + settlements_dist + slope_5 + 
                   streets_dist + wald,
                 method = "rf",
                 trControl = trainControl_LOO,
                 tuneGrid = paramGrid, 
                 data = train_dat,
                 seed=12345)

# Estimate variogram on the residual and return range
train_dat$res <- train_dat$type - predict(mod_LOO, newdata=train_dat) # response is factor ! 
empvar <- variogram(res~1, data = train_dat)
fitvar <- fit.variogram(empvar, vgm(model="Sph", nugget = T))
plot(empvar, fitvar, cutoff=1500, main = "Residual semi-variogram estimation")
(resrange <- fitvar$range[2])

# Compute NNDM indices
(NNDM_indices <- nndm(meuse, meuse.grid, resrange, min_train = 0.5))
#> nndm object
#> Total number of points: 155
#> Mean number of training points: 153.88
#> Minimum number of training points: 150
# Plot NNDM functions
plot(NNDM_indices)

# Evaluate RF model using NDM CV
trainControl_NNDM <- trainControl(method = "cv", savePredictions = T,
                                  index=NNDM_indices$indx_train,
                                  indexOut=NNDM_indices$indx_test)
mod_NNDM <- train(zinc ~ x + y + dist + ffreq + soil,
                  method = "ranger",
                  trControl = trainControl_NNDM,
                  tuneGrid = paramGrid, 
                  data = meuse, 
                  seed=12345)



########
library(spatialsample)

set.seed(1234)
folds <- spatial_clustering_cv(occs, v = 5)

trainids <- CreateSpacetimeFolds(train_dat,spacevar="id",class="type",k=4)

ctrl <- trainControl(method="cv",
                       index=trainids$index)


model_ffs <- ffs(predictors = train_dat[names(train_dat) %in% preds],
                       response = train_dat$type,
                       method="rf",
                       metric = "Kappa",
                       ntree=50,
                       tuneGrid=data.frame("mtry"=2:10),  
                       trControl=trainControl(method="cv",index=trainids$index))

stopCluster(cl)

save(model_ffs,file="ffsmodel.RData") 

plot_ffs(model_ffs)
plot_ffs(model_ffs,plotType="selected")

sel_vars <- model_ffs$selectedvars

rfpred <- predict(model_ffs$finalModel, env_data[sel_vars],
                  type = "prob", cores = detectCores()-1,
                  na.rm = TRUE,
                  filename = "D:/sdm_results/prediction.tif")

library(doParallel) 
library(parallel)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

aoa <- aoa(env_data[sel_vars],model_ffs,cl=cl) 
writeRaster(aoa,"aoa.grd")

# to do: raster of observer density + sample pseudo-absences?





