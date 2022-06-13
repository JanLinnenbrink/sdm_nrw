library(flexsdm)
library(caret)
library(CAST)
library(blockCV)
library(terra)
library(sf)
library(dplyr)
library(purrr)

library(mlr3spatial) # @github
library(mlr3spatiotempcv) # @github
library(mlr3learners)
library(ranger)


# calculate pseudo-absences
train_dat <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_dat.rds")
env_data_sr <- terra::rast("D:/env_data/env_data_sr.tif")
nrw <- st_read("C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw.shp")
occs <- st_read("D:/env_data/occs.shp")

occs_xy <- occs %>%
  mutate(x=unlist(map(.$geometry,1)), y = unlist(map(.$geometry,2))) %>% 
  select(x,y) %>% 
  st_drop_geometry() %>% 
  as.data.frame()

env_pa <- terra::aggregate(env_data_sr, 10)

rm(env_data_sr); rm(occs)



plot(occs_xy$x, occs_xy$y)
plot(pa$x,pa$y, col="red")

rlayer <- env_pa[[1]] 
values(rlayer) <- 1 #all values to one

writeRaster(env_pa, "D:/env_data/env_pa.tif", overwrite=TRUE)
#env_pa <- rast("D:/env_data/env_pa.tif")

# sample pseudo-absence data
pa <- flexsdm::sample_pseudoabs(occs_xy, "x", "y", n=1000, 
                          method=c("random"),
                          rlayer=rlayer)

##
# loading the package
library(randomForest)

# convert the response to factor for producing class relative likelihood
training$occ <- as.factor(training$occ)

prNum <- as.numeric(table(training$occ)["1"]) # number of presences

# the sample size in each class; the same as presence number
smpsize <- c("0" = prNum, "1" = prNum)


rf_downsampled <- randomForest(formula = occ ~., 
                              data = training, 
                              ntree = 1000, 
                              sampsize = smpsize,
                              replace = TRUE)

rfpred <- predict(rf_downsampled, testing_env, type = "prob")

plot(rf_downsample, main = "RF down-sampled")


###
#train learner
task_train = as_task_classif(point_vector, id = "sites_muenster", target = "class")

learner = lrn("classif.ranger", importance.mode = "impurity")

resampling = rsmp("sptcv_cstf", folds = 3, stratify = TRUE)
rr = resample(task_train, learner, resampling)
rr$aggregate()

learner$train(task_train)

learner$model

task_predict = as_task_classif(stack, id = "sentinel_muenster", target = "class", task_train = task_train)
future::plan("multisession", workers = 4)
learner$parallel_predict = TRUE

raster_muenster = predict_spatial(task_predict, learner, chunksize = 10L, filename = "raster_muenster.tif")

library(mlr3fselect)

#spatial ffs
instance = fselect(
  method = "sequential", #ffs
  task =  tsk("diplodia"),
  learner = lrn("classif.rpart"),
  resampling = rsmp("spcv_coords"),
  measure = msr("classif.ce"),
  batch_size = 5
)

instance$result

# Hyperparameter Tuning.


library(mlr3tuning)
library(mlr3tuningspaces) # @github

instance = tune(
  method = "random_search",
  task = tsk("diplodia"),
  learner = lts(lrn("classif.rpart")),
  resampling = rsmp("spcv_coords"),
  measure = msr("classif.ce"),
  term_evals = 10
)

# best performing hyperparameter configuration
instance$result
