cv_estimation <- function(pts=pts_clustered, predictors=predictors_global,
                          maxn = 36) {
  
  
  predictors <- rast(predictors)
  
  # possible n of cv folds
  possible_n <- c(4:sqrt(maxn))^2
  
  ### loop create cv folds based on different block size
  cv_dist <- function(n) {
    
    # get geographical extent of predictors
    ug <- ext(predictors[[1]])
    ug <- st_as_sf(as.polygons(ug))
    st_crs(ug) <- st_crs(predictors[[1]])
    
    # create tile polygon
    ug_tiles <- st_make_grid(ug, n=sqrt(possible_n[[n]]))
    ug_df <- data.frame(n = 1:possible_n[[n]])
    ug_df$geom <- st_sfc(ug_tiles)
    ug_tiles <- st_as_sf(ug_df)
    
    # assign fold id
    pts_id <- st_join(pts, ug_tiles)
    spatialfolds <- CreateSpacetimeFolds(pts_id,spacevar="n",k=length(unique(pts_id$n)))
    dist_clstr_sCV <- plot_geodist(pts,stack(predictors),
                                   type = "feature", sampling="regular",
                                   cvfolds = spatialfolds$indexOut,
                                   showPlot=FALSE)
    
    s2s <- dist_clstr_sCV$distances[dist_clstr_sCV$distances$what=="sample-to-sample",]$dist
    s2p <- dist_clstr_sCV$distances[dist_clstr_sCV$distances$what=="sample-to-prediction",]$dist
    cv2p <- dist_clstr_sCV$distances[dist_clstr_sCV$distances$what=="CV-distances",]$dist
    
    
    ks_s2s <- ks.test(s2s,s2p)
    ks_cv <- ks.test(cv2p,s2p)
    return(ks_cv)
  }
  
  # access difference to sample-to-prediction distribution for each block size
  # and stop when k test is not significant
  
  ds <- lapply(c(1:length(possible_n)), function(x) {
    dist <- cv_dist(x)
  }
  )
  
  ds_df <- lapply(ds,function(x) as.data.frame(x$statistic[[1]]))
  ds_df <- do.call(rbind, ds_df)
  ds_df$size <- possible_n
  n_sel <- ds_df[ds_df$`x$statistic[[1]]`==min(ds_df$`x$statistic[[1]]`),][["size"]]
  
  # repeat step 1 with chosen block size
  cv_dist_sel <- function(n_sel=n_sel) {
    
    n = n_sel
    # get geographical extent of predictors
    ug <- terra::ext(predictors[[1]])
    ug <- st_as_sf(terra::as.polygons(ug))
    st_crs(ug) <- st_crs(predictors[[1]])
    
    # create tile polygon
    ug_tiles <- st_make_grid(ug, n=sqrt(n_sel))
    ug_df <- data.frame(n = 1:n_sel)
    ug_df$geom <- st_sfc(ug_tiles)
    ug_tiles <- st_as_sf(ug_df)
    
    # assign fold id
    pts_id <- st_join(pts, ug_tiles)
    spatialfolds <- CreateSpacetimeFolds(pts_id,spacevar="n",k=length(unique(pts_id$n)))
    dist_clstr_sCV <- plot_geodist(pts,stack(predictors),
                                   type = "feature", sampling="regular",
                                   cvfolds = spatialfolds$indexOut,
                                   showPlot=FALSE)
    out <- list("distances" = dist_clstr_sCV, "points" = pts_id,
                "blocks" = ug_tiles)
    return(out)
  }
  dsc <- cv_dist_sel(n_sel = n_sel)
  # points: n = folds
  return(dsc)
  
}
