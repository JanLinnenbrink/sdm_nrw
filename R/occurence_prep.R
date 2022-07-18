require(sf)
require(raster)
require(ggplot2)
library(terra)
library(dplyr)
library(dismo)
library(flexsdm)



# load data

env_data <- rast("d:/env_data.tif")

occs <- st_read("d:/env_data/occs.shp") %>% st_transform(st_crs(env_data))
bg_sf <- st_read("e:/sdm_nrw/bg_vect.shp")
bg_vect <- vect(bg_sf)

train_dat <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_dat_wobg.rds")
train_dat_c <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_data.rds")




# sample background points
set.seed(1)
bg <- dismo::randomPoints(raster(env_data[[1]]), 5000)
bg_vect <- vect(bg, type = "points", crs = terra::crs(env_data))
bg_sf <- st_as_sf(bg_vect)


# spatial cv grid
bg_bb <- st_bbox(bg_sf)
gr <- st_make_grid(bg_bb, n = 2)
plot(gr)

grsf <- st_sf(gr, id=c(1:4))

occs_id <- st_join(occs, grsf)
bg_id <- st_join(bg_sf, grsf)

occs <- occs[names(occs) %in% "geom"]
occs$id <- 1:nrow(occs)

occs_vect <- occs %>% 
  st_transform(st_crs(env_data)) %>% 
  vect()


bg_vect <- vect(bg_sf)

train_dat <- terra::extract(env_data, occs_vect)[, names(env_data)]
train_dat$type <- factor("presence")

train_dat$id <- 1:nrow(occs_vect) 
extr <- merge(train_dat,occs) %>% st_as_sf()

st_geometry(extr) <-  "geometry"

readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/extr.rds")

bg_dat <- terra::extract(env_data, bg_vect, df=TRUE)[, names(env_data)]
bg_dat$type <-  factor("background")
bg_dat$id <-1:nrow(bg_dat) 
bg_dat$FID <- NULL

extr_bg <- merge(bg_dat,bg_sf) %>% st_as_sf()

train_dat_c <- rbind(extr, extr_bg)

train_dat_c <- train_dat_c[-"bahn_5"]

st_write(train_dat_c, "train_dat.gpkg")
saveRDS(train_dat_c, "C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_data_new_sf.rds")
terra::writeVector(bg_vect, "C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/bg_vect.shp")


plot(train_dat_c$slope_5)


ggplot(train_dat_c) +
  geom_density(aes(x=aspect_5, colour = type)) 





# create pseudo-absences

data <- st_coordinates(occs)
rlayer <- env_data[[1]]


pa_geo <- sample_pseudoabs(data=data, x="X", y="Y", n=5000,
                           method = c("geo_env_km_const", width = "500", env = env_data),
                           rlayer=rlayer, maskval = NULL)


# map occurences
germany <- getData("GADM", country = "Germany", level = 2) %>% 
  st_as_sf() %>% 
  st_transform(st_crs(31468))

ndvi <- terra::aggregate(env_data[[7]], fact=10, cores = 6)


ocp <- ggplot() +
    layer_spatial(data=raster(ndvi)) +
    geom_sf(data=st_as_sf(bg_vect), shape = 1, size=0.8, alpha=0.5) +
  geom_sf(data=occs, shape = 21, fill="blue4", size=1.4)
  
ggsave("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/Feldgrille_plots/occs_bgrandom_plot1.pdf", ocp)


# calculate observer density


