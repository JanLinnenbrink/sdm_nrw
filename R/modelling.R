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


### load data


#occs_prep(occurences_sf = occs, env_data = env_data, 
#          path_out = "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/occurence_data")

train_dat <- readRDS("e:/sdm_nrw/train_dat")

preds_n <- names(train_dat[!names(train_dat) %in% c("type", "ID")])

### create spatial cv folds
set.seed(1234)

occs <- st_read("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/R/occurences.gpkg") %>%
  mutate(FID = 1:nrow(.)) %>% 
  .[,"FID"]
bgs <- st_read("E:/sdm_nrw/bg_vect.shp")

bgs_s <- bgs[sample(nrow(bgs), nrow(occs)),]

v <- 4
coords <- sf::st_centroid(sf::st_geometry(occs))
dists <- as.dist(sf::st_distance(coords))
km <- kmeans(dists, centers = v)
occs$cl <- km$cluster

samplennb <- GeoInterpolation::doNearestNeighbor(cl~1,occs,bgs_s)

df <- data.frame(pres = rep(1,nrow(occs)))
occs_g <- st_sf(df, st_geometry(occs))
occs_g$cl <- occs$cl
st_geometry(occs_g) <- "geom"

df <- data.frame(pres = rep(0,nrow(bgs_s)))
background_g <- st_sf(df, st_geometry(bgs_s))
background_g$cl <- samplennb$var1.pred
st_geometry(background_g) <- "geom"

points <- rbind(occs_g,background_g)

st_write(points, "E:/sdm_nrw/training_points.shp", append=FALSE)


ggplot(points, aes(alpha = as.factor(pres), colour=as.factor(cl))) +
  geom_sf()


trainDat <- data.frame(cl=points$cl, pres = points$pres,
                       terra::extract(predictors, vect(points), df=TRUE))%>% 
  .[complete.cases(.),]

saveRDS(trainDat, "E:/sdm_nrw/trainDat")


### ffs
indices <- CreateSpacetimeFolds(trainDat,spacevar = "cl",clas="pres",k=5)
ctrl <- trainControl(method="cv",index=indices)

library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

set.seed(1234)
model_ffs <- ffs(predictors = trainDat[,names(trainDat) %in% names(predictors)],
                 response = trainDat$pres,
                 importance=TRUE,
                 tuneGrid=data.frame("mtry"=1:length(names(predictors))),
                 method="rf",
                 trControl=trainControl(method="cv",index=indices$index))

stopCluster(cl)

ggplot(varImp(model_ffs)) +
  geom_bar(width=1, stat="identity")

saveRDS(model_ffs,file="E:/sdm_nrw/ffs_model") 
final_model <- model_ffs$finalModel

plot_ffs(model_ffs)
plot_ffs(model_ffs,plotType="selected")

sel_vars <- model_ffs$selectedvars

writeRaster(predictors, "E:/sdm_nrw/predictors_aktuell.tif")

model_ffs <- readRDS("E:/sdm_nrw/ffs_model")


