require(sf)
require(raster)
require(ggplot2)
library(terra)
library(dplyr)
library(dismo)
library(flexsdm)

setwd("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw")

#predictors <- rast("e:/sdm_nrw/predictors.grd")

occs <- st_read(paste0(getwd(), "/R/occurences.gpkg")) %>%
  st_transform(st_crs(predictors[[1]])) %>% 
  mutate(FID = 1:nrow(.)) %>% 
  .[,"FID"]
bg <- st_read(paste0(getwd(), "/bg_data/bg_vect.shp"))


setwd("C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw")
# sample background points
#set.seed(1)
#bg <- dismo::randomPoints(raster(predictors[[1]]), 5000)
#bg_vect <- vect(bg, type = "points", crs = terra::crs(predictors))
#bg_sf <- st_as_sf(bg_vect)


occs_id <- st_join(occs, grsf)
bg_id <- st_join(bg, grsf)
occs_id$FID = bg_id$FID <- NULL

occs_vect <- occs %>% 
  st_transform(st_crs(predictors)) %>% 
  vect()

bg_vect <- vect(bg)

extr <- terra::extract(predictors, occs_vect, df=TRUE)
extr$type <- factor("presence") 
extr <- merge(extr,occs, by.x="ID", by.y="FID") %>% st_as_sf()
st_geometry(extr) <-  "geometry"

bg_extr <- terra::extract(predictors, bg_vect, df=TRUE)
bg_extr$type <-  factor("background")

bg_extr <- merge(bg_extr,bg, by.x="ID", by.y="FID") %>% st_as_sf()

train_dat_sf <- rbind(extr, bg_extr)
train_dat <- st_drop_geometry(train_dat_sf) %>% 
  .[complete.cases(.),]

saveRDS(train_dat, "e:/sdm_nrw/train_dat")
train_dat <- readRDS("train_dat")

ggplot(train_dat) +
  geom_density(aes(x=ndvi, colour = type)) 



# create pseudo-absences

data <- st_coordinates(occs)
rlayer <- predictors[[1]]


pa_geo <- sample_pseudoabs(data=data, x="X", y="Y", n=5000,
                           method = c("geo_env_km_const", width = "500", env = predictors),
                           rlayer=rlayer, maskval = NULL)


# map occurences
germany <- getData("GADM", country = "Germany", level = 2) %>% 
  st_as_sf() %>% 
  st_transform(st_crs(31468))

ndvi <- terra::aggregate(predictors[[7]], fact=10, cores = 6)


ocp <- ggplot() +
    layer_spatial(data=raster(ndvi)) +
    geom_sf(data=st_as_sf(bg_vect), shape = 1, size=0.8, alpha=0.5) +
  geom_sf(data=occs, shape = 21, fill="blue4", size=1.4)
  
ggsave("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/Feldgrille_plots/occs_bgrandom_plot1.pdf", ocp)


# calculate observer density


