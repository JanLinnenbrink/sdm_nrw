require(sf)
require(raster)
require(ggplot2)
library(terra)
library(dplyr)
library(dismo)

# load env data & calculate focal statistics
#setwd("D:/env_data/resampled_old") # eigentlich ohne letzten folder
setwd("D:/env_data/") 

files <- list.files(pattern = "*.tif$")

nrw <- germany[germany$NAME_1 == "Nordrhein-Westfalen",]%>% 
  st_transform(st_crs(25832))
nrw <- st_union(nrw)
nrw_spat <- terra::vect(nrw)
st_write(nrw, "C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw.shp")

nrw <- st_read("C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw.shp")
nrw <- vect(nrw)

#remove <- c("env_data.tif", "env_data_masked.tif", "env_data_sr.tif",
#            "env_pa.tif", "masked_env_data.tif", "rlayer.tif", "slope_5_res.tif")
#files <- files[! files %in% remove]

rasters <- lapply(files, rast)

#rasters$ndvi_sd <- terra::focal(rasters$ndvi_f.tif, fun = "sd", filename ="ndvi_sd.tif")
#rasters$ndvi_sd <- terra::crop(rasters$ndvi_sd, nrw_spat)

dir.create("resampled")

raster_res <- list()
for (i in seq_along(rasters)) {
  raster_res[[i]] <- terra::resample(x=rasters[[i]], y=rasters[[1]],
                                     filename=paste0("resampled/",names(rasters[[i]]), "_res.tif"),
                                     overwrite=TRUE)
}

rasterfiles <- list.files("resampled/")
env_data <- terra::rast(rasterfiles)

terraOptions(tempdir = "D:/env_data/resampled_old")
env_data <- terra::mask(env_data, nrw)

terra::writeRaster(env_data, "env_data_masked1.tif")

# load function wsl.ebc for environmental bias correction
source(paste0(getwd(), "/wsl_ebc.R"))

occs <- st_read("occs.shp")

#occs_prep <- function(occurences_sf, env_data, path_out) {
  
  occs_xy <- occurences_sf %>%
    mutate(x=unlist(map(occurences_sf$geometry,1)), y = unlist(map(occurences_sf$geometry,2))) %>% 
    select(x,y) %>% 
    st_drop_geometry() %>% 
    as.data.frame()
  
  ggplot() +
    geom_point(data=occs_xy, mapping=aes(x=x, y=y, colour=NULL))
  
  wsl.ebc(obs = occs_xy,
          ras = env_data,
          pportional = TRUE,
          plog = TRUE,
          nclust = 5,
          sp.specific = FALSE,
          filter = TRUE,
          keep.bias = FALSE,
          path = path_out)
  
  files <- list.files(path_out)
  target_files <- files[grep("_obs_corrected_", files)]
  obs_correct <- lapply(target.files, function(x) obs=read.table(paste0(path_out,"/",x)))
  obs_correct <- do.call("rbind", obs_correct)
}

#occs_prep(occurences_sf = occs, env_data = env_data, 
#          path_out = "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/occurence_data")


# convert to vect & spatrast
# env_data <- rast("env_data_masked1.tif")



# reclassify binary data (necessary due to resampling)
m <- c(0,0,0, 0.1,255,1)
rm <- matrix(m, ncol=3, byrow=TRUE)

env_data$acker <- terra::classify(env_data$acker, rm, include.lowest=TRUE)
env_data$bahn_5 <- terra::classify(env_data$bahn_5, rm, include.lowest=TRUE)
env_data$wald <- terra::classify(env_data$wald, rm, include.lowest=TRUE)

terra::writeRaster(env_data, "env_data_aktuell1.tif")

env_data <- rast("env_data_aktuell1.tif")

occs_vect <- occs %>% 
  st_transform(st_crs(env_data)) %>% 
  vect()


train_dat <- terra::extract(env_data, occs_vect)[, names(env_data)]
train_dat$type <- factor("presence")

saveRDS(train_dat, "C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_dat_wobg.rds")
train_dat <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_dat_wobg.rds")

set.seed(1)
bg <- spatSample(env_data$acker, 5000, "random")

bg <- dismo::randomPoints(raster(env_data[[1]]), 5000)
bg_vect <- vect(bg, type = "points", crs = terra::crs(env_data))

bg_dat <- terra::extract(env_data, bg_vect)[, names(env_data)]
bg_dat$type <-  factor("background")

train_dat_c <- rbind(train_dat, bg_dat)

saveRDS(train_dat_c, "C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_data.rds")
terra::writeVector(bg_vect, "bg_vect.shp")


ggplot(train_dat_c) +
  geom_density(aes(x=aspect_5, colour = type)) 

# map occurences
germany <- getData("GADM", country = "Germany", level = 2) %>% 
  st_as_sf() %>% 
  st_transform(st_crs(31468))


ocp <- ggplot() +
  geom_sf(data=germany[germany$NAME_1=="Nordrhein-Westfalen",]) +
  geom_sf(data=occs, shape = 21, colour="blue4", size=1.4)
  
ggsave("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/Feldgrille_plots/occs_plot.pdf", ocp)


