library(CAST)
library(caret)
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)

predictors_global <- stack(system.file("extdata","bioclim_global.grd",package="CAST"))
source("C:/0_Msc_Loek/M7_Fernerkundung/clustered_sample.R")
source("C:/0_Msc_Loek/M7_Fernerkundung/sampleFromArea.R")
source("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/R/cv_estimation.R")


seed <- 10 # random realization
samplesize <- 300 # how many samples will be used?
nparents <- 20 #For clustered samples: How many clusters? 
radius <- 7 # For clustered samples: What is the radius of a cluster?


# prediction area (Global)
ee <- st_crs("+proj=eqearth")
co <- ne_countries(returnclass = "sf")
co.ee <- st_transform(co, ee)


# spatial clustered sample
set.seed(seed)
sf_use_s2(FALSE)
pts_clustered <- clustered_sample(co, samplesize, nparents, radius)





#### function (terra and sf based)

cds <- cv_estimation(pts=pts_clustered, predictors=predictors_global)


