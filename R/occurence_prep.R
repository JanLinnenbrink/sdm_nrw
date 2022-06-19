require(sf)
require(raster)
require(ggplot2)
library(terra)
library(dplyr)
library(dismo)
library(flexsdm)



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


occs_vect <- occs %>% 
  st_transform(st_crs(env_data)) %>% 
  vect()


train_dat <- terra::extract(env_data, occs_vect)[, names(env_data)]
train_dat$type <- factor("presence")

saveRDS(train_dat, "C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_dat_updated.rds")
train_dat <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_dat_wobg.rds")

set.seed(1)
bg <- dismo::randomPoints(raster(env_data[[1]]), 5000)
bg_vect <- vect(bg, type = "points", crs = terra::crs(env_data))

bg_vect <- vect("bg_vect.shp")

bg_dat <- terra::extract(env_data, bg_vect)[, names(env_data)]
bg_dat$type <-  factor("background")

train_dat_c <- rbind(train_dat, bg_dat)

saveRDS(train_dat_c, "C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_data_updated.rds")
terra::writeVector(bg_vect, "bg_vect.shp")

train_dat_c <- readRDS("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/train_data.rds")

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


ocp <- ggplot() +
  geom_sf(data=germany[germany$NAME_1=="Nordrhein-Westfalen",]) +
  geom_sf(data=occs, shape = 21, colour="blue4", size=1.4)
  
ggsave("C:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/Feldgrille_plots/occs_plot.pdf", ocp)


