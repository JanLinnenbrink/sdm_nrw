sample_pseudoabs(occs_xy, "x", "y", n=1000, 
                 method=c("geo_env_km_const", width = "100", env = env_pa),
                 rlayer=rlayer)

x="x"
y="y"

sample_pseudoabs <- function(data, x, y, n, method, rlayer, maskval = NULL, calibarea = NULL, sp_name = NULL) {
  . <- ID <- NULL
  
  if (!any(c(
    "random",
    "env_const",
    "geo_const",
    "geo_env_const",
    "geo_env_km_const"
  ) %in% method)) {
    stop(
      "argument 'method' was misused, available methods random, env_const, geo_const, geo_env_const, and geo_env_km_const"
    )
  }
  
  rlayer <- rlayer[[1]]
  data <- occs_xy[, c(x,y)]
  
 
  # geo_env_km_const
  if (any(method == "geo_env_km_const")) {
    
    env <- method[["env"]]
    
    # Restriction for a given region
    envp <- inv_geo(e = rlayer, p = data[, c(x, y)], d = 100)
    envp2 <- inv_bio(e = env, p = data[, c(x, y)])
    envp <- (envp2 + envp)
    envp <- terra::mask(rlayer, envp)
    rm(envp2)
    
    if (!is.null(maskval)) {
      if (is.factor(maskval)) {
        maskval <-
          which(levels(maskval) %in% as.character(maskval))
        rlayer <- rlayer * 1
      }
      filt <- terra::match(rlayer, maskval)
      rlayer <- terra::mask(rlayer, filt)
      rm(filt)
    }
    
    envp <- terra::mask(rlayer, envp)
    
    # K-mean procedure
    env_changed <- terra::mask(env, envp)
    env_changed <- terra::as.data.frame(env_changed, xy = TRUE)
    env_changed <- stats::na.exclude(env_changed)
    
    suppressWarnings(km <- stats::kmeans(env_changed, centers = n))
    cell_samp <- km$centers[, 1:2] %>% data.frame()
    val <- terra::extract(envp, cell_samp, method = "simple", xy = TRUE) %>%
      dplyr::select(-c(ID, x, y))
    cell_samp <-
      cell_samp %>% dplyr::mutate(val = val[, 1])
    cell_samp <- cell_samp[!is.na(cell_samp$val), -3]
    cell_samp <- dplyr::tibble(cell_samp)
    cell_samp$pr_ab <- 0
  }
  colnames(cell_samp) <- c(x, y, "pr_ab")
  if(!is.null(sp_name)){
    cell_samp <- tibble(sp=sp_name, cell_samp)
  }
  return(cell_samp)
}