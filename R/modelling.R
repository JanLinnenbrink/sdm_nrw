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
library(mlr3fselect)
library(mlr3tuning)
library(mlr3tuningspaces) # @github
library(ranger)
library(randomForest)
library(dplyr)

source(paste0(getwd(), "/wsl_ebc.R"))


# occurence filtering
occs <- st_read("d:/env_data/occs.shp") %>% st_transform(st_crs(env_data[[1]]))
env_data <- rast("d:/env_data/env_data_aktuell1.tif")
path_out <- "C:/0_Msc_Loek/M7_Fernerkundung/"


occs_xy <- as.data.frame(st_coordinates(occs))
  
wsl.ebc(obs = occs_xy,
          ras = stack(env_data),
          pportional = TRUE,
          plog = TRUE,
          nclust = 5,
          sp.specific = FALSE,
          filter = TRUE,
          keep.bias = FALSE,
          path = "C:/0_Msc_Loek/M7_Fernerkundung")
  
files <- list.files("C:/0_Msc_Loek/M7_Fernerkundung")
target_files <- files[grep("_obs_corrected_", files)]
obs_correct <- lapply(target.files, function(x) obs=read.table(paste0(path_out,"/",x)))
obs_correct <- do.call("rbind", obs_correct)


#occs_prep(occurences_sf = occs, env_data = env_data, 
#          path_out = "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/occurence_data")

train_dat <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_data_new.rds")
train_dat <- train_dat[complete.cases(train_dat),]

library(caret)
library(CAST)
library(ROSE)

id <- as.data.frame(train_dat[["id"]])


bg <- train_dat[train_dat$type == "background",]
occ <- train_dat[train_dat$type == "presence",]


bg_sample <- bg[sample(nrow(bg), nrow(occ)),]

train_dat <- rbind(bg_sample, occ)


trainids <- CreateSpacetimeFolds(train_dat,spacevar="id",class="type",k=4)

ctrl <- trainControl(method="cv",
                       index=trainids$index)

preds <- names(train_dat[!names(train_dat) %in% c("type", "id")])

model_ffs <- CAST::ffs(predictors = train_dat[names(train_dat) %in% preds],
                       response = train_dat$type,
                       method="rf",
                       metric = "Kappa",
                       ntree=50,
                       tuneGrid=data.frame("mtry"=2:10),  
                       trControl=trainControl(method="cv",index=trainids$index))

save(model_ffs,file="ffsmodel.RData") 

plot_ffs(model_ffs)
plot_ffs(model_ffs,plotType="selected")

sel_vars <- model_ffs$selectedvars

rfpred <- predict(model_ffs$finalModel, env_data[sel_vars],
                  type = "prob", cores = detectCores()-1)

library(doParallel) 
library(parallel)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

aoa <- aoa(env_data[sel_vars],model_ffs,cl=cl) # wenn Ohne Parallelprozessierung: CL rausnehmen!
writeRaster(aoa,"aoa.grd")

# to do: raster of observer density + sample pseudo-absences?

library(raster)
library(terra)
env_data$acker <- as.factor(env_data$acker)
levels(env_data$acker) <- c("no_cultiv", "cultiv")
env_data$bahn_5 <- as.factor(env_data$bahn_5)
levels(env_data$bahn_5) <- c("no_railw", "railw")
env_data$wald <- as.factor(env_data$wald)
levels(env_data$wald) <- c("no_forest", "forest")


terra::writeRaster(env_data, "D:/env_data.tif")



