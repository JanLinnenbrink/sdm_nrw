require(sf)
require(raster)
require(cluster)
require(ggplot2)
library(terra, exclude = "resample")
library(mlr3spatial) # @github
library(mlr3spatiotempcv) # @github
library(mlr3learners)
library(ranger)

# load function wsl.ebc for environmental bias correction
source(paste0(getwd(), "/wsl_ebc.R"))

occs_prep <- function(path2occs,x,y, env_data) {
  occs <- read.csv(path2occs)
  occs_xy <- occs[,c(x,y)]
  
  ggplot() +
    geom_point(data=occs_xy, mapping=aes(x=x, y=y, colour=NULL))
  
  wsl.ebc(obs = occs,
          ras = env_data,
          pportional = TRUE,
          plog = TRUE,
          nclust = 50,
          sp.specific = FALSE,
          sp.cor = 0.5,
          filter = TRUE,
          keep.bias = FALSE,
          path = path2occs)
  
  files <- list.files(path2occs)
  target_files <- files[grep("_obs_corrected_", files)]
  obs_correct <- lapply(target.files, function(x) obs=read.table(paste0(path2occs,"/",x)))
  obs_correct <- do.call("rbind", obs_correct)
}


setwd("D:/env_data/")

occs <- st_read("occs.shp")


# map occurences
germany <- getData("GADM", country = "Germany", level = 2) %>% 
  st_as_sf() %>% 
  st_transform(st_crs(31468))

nrw <- germany[germany$NAME_1 == "Nordrhein-Westfalen",]%>% 
  st_transform(st_crs(25832))
nrw <- st_union(nrw)
st_write(nrw, "C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw.shp")

ocp <- ggplot() +
  geom_sf(data=germany[germany$NAME_1=="Nordrhein-Westfalen",]) +
  geom_sf(data=occs, shape = 21, colour="blue4", size=1.4)
  
ggsave("occs_plot.png", ocp)


# load env data
files <- list.files(pattern = "*.tif$")
rasters <- lapply(files, rast)
names(rasters) <- list.files(pattern = "*.tif$")
rasters$ndvi_sd <- terra::focal(rasters$ndvi_f.tif, fun = "sd", filename ="ndvi_sd.tif")

raster_res <- lapply(rasters, resample, rasters$dgm_5.tif)

env_data <- stack(raster_res)

# obs = data frame with 3 columns: x, y, sp.id
# ras = predictors
# Run EBC function with the log consensus
# sp.specific = TRUE --> Select corrected XY outputs in a species-specific manner
# keep.bias = TRUE   --> Preserve initial observer bias of each species
wsl.ebc(obs = occs_xy,
        ras = env_data,
        pportional = TRUE,
        plog = TRUE,
        nclust = 50,
        sp.specific = FALSE,
        sp.cor = 0.5,
        filter = TRUE,
        keep.bias = FALSE,
        path = getwd())


# Open corrected observations
files = list.files(getwd())
target.files = files[grep("_obs_corrected_",files)]
correct.obs = lapply(target.files, function(x) obs=read.table(paste0(getwd(),"/",x)))
correct.obs = do.call("rbind",correct.obs)


# convert to vect
occs_vect <- vect(occs_sf)

train_dat <- extract(env_data, occs_vect)[, names(env_data)]
values(point_vector) <- cbind(values(point_vector), data)

plot(env_data[[2]])
plot(occs_vect)

# to sf
point_vector = st_as_sf(point_vector)
point_vector["class"] = factor(point_vector[["class"]])
point_vector["polygon"] = factor(point_vector[["polygon"]])

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