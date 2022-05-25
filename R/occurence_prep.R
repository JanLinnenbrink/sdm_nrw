require(sf)
require(raster)
require(cluster)

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




# obs = data frame with 3 columns: x, y, sp.id
# ras = predictors
# Run EBC function with the log consensus
# sp.specific = TRUE --> Select corrected XY outputs in a species-specific manner
# keep.bias = TRUE   --> Preserve initial observer bias of each species
wsl.ebc(obs = occs,
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

library(ggplot2)





