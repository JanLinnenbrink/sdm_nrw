require(sf)
require(raster)
require(cluster)
require(ggplot2)

# load function wsl.ebc for environmental bias correction
source(paste0(getwd(), "/R/wsl_ebc.R"))

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

occs <- read.csv("C:/Users/janli/sciebo/FE_22_Citizen_Science/data/occurence_data/occurences.csv")
occs_xy <- occs[,c("X31468.X", "X31468.Y")]

occs_xy$x <- as.numeric(gsub(",", "", occs_xy$X31468.X))
occs_xy$y <- as.numeric(gsub(",", "", occs_xy$X31468.Y))
occs_xy$X31468.X <- NULL
occs_xy$X31468.Y <- NULL

occs_sf <- st_as_sf(occs_xy, coords = c("x","y"), crs = st_crs(31468))

nd <- env_data[[1]] * 2

# map occurences

germany <- getData("GADM", country = "Germany", level = 2) %>% 
  st_as_sf() %>% 
  st_transform(st_crs(31468))

ocp <- ggplot() +
  geom_sf(data=germany[germany$NAME_1=="Nordrhein-Westfalen",]) +
  geom_sf(data=occs_sf, shape = 21, colour="blue4", size=1.4)
  
ggsave("occs_plot.png", ocp)


# load env data

env_data <- stack("")

# obs = data frame with 3 columns: x, y, sp.id
# ras = predictors
# Run EBC function with the log consensus
# sp.specific = TRUE --> Select corrected XY outputs in a species-specific manner
# keep.bias = TRUE   --> Preserve initial observer bias of each species
wsl.ebc(obs = occs_xy,
        ras = rst[[1:5]],
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


