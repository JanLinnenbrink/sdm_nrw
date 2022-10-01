library(terra)
library(sf)
library(mlr3)
library(mlr3learners)
library(mlr3spatial)
library(mlr3spatiotempcv)
library(mlr3extralearners)
library(mlr3viz)
library(ggplot2)
library(iml)

# specify path to files (scratch)
path_in <- "e:/sdm_nrw/model_results/"
path_out_temp <- "C:/0_Msc_Loek/Z_Palma/results/out_temp"
path_out <- "C:/0_Msc_Loek/Z_Palma/results/prediction_aoa/"

dir.create(path_out)

terraOptions(tempdir=path_out_temp, overwrite=TRUE)

ncores <- 9

### load predictors ###
predictors <- lapply(list.files(path_out_temp, pattern="*.tif$", full.names = TRUE), rast) 

### modelling with mlr3 ###
trainDat <- readRDS(paste0(path_in, "trainDat_st"))

trainDat$cl <- as.factor(trainDat$cl)
trainDat$pres <- as.factor(ifelse(trainDat$pres==1,"presence", "absence"))
trainDat$forest <- as.factor(trainDat$forest)
trainDat$cultiv <- as.factor(trainDat$cultiv)


# cross-validation
cd <- readRDS("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/dist_new")

length(unique(cd$points$n))
points <- st_read("E:/sdm_nrw/training_points.shp")

bg <- points[points$pres == 0, ]
samplennb <- GeoInterpolation::doNearestNeighbor(n~1,cd$points,bg)

df <- data.frame(pres = rep(1,nrow(cd$points)))
occs_g <- st_sf(df, st_geometry(cd$points))
occs_g$cl <- cd$points$n
st_geometry(occs_g) <- "geom"

df <- data.frame(pres = rep(0,nrow(bg)))
background_g <- st_sf(df, st_geometry(bg))
background_g$cl <- samplennb$var1.pred
st_geometry(background_g) <- "geom"

points <- rbind(occs_g,background_g)
points$cl <- as.factor(points$cl)
levels(points$cl) <- c(1:length(unique(points$cl)))
#st_write(points, "e:/sdm_nrw/points_newcv.shp", append=FALSE)
points <- st_read("e:/sdm_nrw/points_newcv.shp")

points_df <- data.frame(x = as.data.frame(st_coordinates(points))$X,
                        y = as.data.frame(st_coordinates(points))$Y,
                        cl = points$cl)

trainDat <- trainDat |> subset(select = -cl)

trainDat_cv <- dplyr::inner_join(trainDat, points_df, by = c("x", "y"))

#trainDat_cv <- trainDat_cv |> subset(select = -x) |> subset(select=-y)
trainDat_cv$cl <- as.factor(trainDat_cv$cl)

# train
set.seed(100)
backend <- as_data_backend(trainDat_cv[,names(trainDat_cv) != "cl"], keep_rownames = FALSE)

task <- as_task_classif_st(backend, id = "training_points", target = "pres",
                           coordinate_names = c("x","y"), coords_as_features = FALSE,
                           crs = 25832)

lrn <- lrn("classif.randomForest", 
           importance = "accuracy", 
           mtry=2L, predict_type = "prob")

lrn$train(task)
lrn$model

### resampling ###
# manual
custom_cv = rsmp("custom_cv")
f = as.factor(trainDat_cv$cl)
custom_cv$instantiate(task, f = f)

msr <- as.data.table(mlr_measures) 
msr_prob <- msr[msr$predict_type == "prob", ]

rr <- mlr3::resample(task, lrn, custom_cv, store_models = TRUE)

rr_res <- lapply(c(1:nrow(msr_prob)), function(x) rr$aggregate(msr(msr_prob$key[[x]]))) |> 
  unlist() |> 
  as.data.frame()

ncol(rr_res)

(roc <- autoplot(rr, type= "roc"))
roc_plot <- roc + annotate("text", x = 0.8, y = 0.2, 
                           label = paste("mean AUC =",
                                         round(rr$aggregate(measures=mlr_measures$get("classif.auc")),2))) 

ggsave(paste0(path_out, "roc.pdf"), roc_plot)

rr <- readRDS("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/rr")

# model plots
library(dplyr)
library(randomForest)

lrn <- readRDS("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/lrn")
task <- readRDS("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/task")

trainDat <- trainDat |> subset(select = -x) |> subset(select=-y) 
trainDat <- trainDat[complete.cases(trainDat),]
x <- trainDat[which(names(trainDat) != "pres")]

model = Predictor$new(lrn$model, data = x, y = trainDat$pres)
prediction = lrn$predict(task)

autoplot(prediction)
autoplot(task, type = "pairs")

cl_pl <- plot(rcv, task)
cl_pl <- cl_pl + labs(title="spatial clustering")
custom_pl <- plot(custom_cv, task) 
custom_pl <- custom_pl + labs(title="tuned spatial block")

(both_cv <- cl_pl + custom_pl)
ggsave(paste0(path_out, "cv_cl.pdf"), both_cv)

# Precision/recall graph
library(patchwork)
effect = FeatureEffects$new(model)
(effect_p <- plot(effect)+
    plot_annotation(
      title = element_blank()) &
    theme(text = element_text(size = 14)))


imp = FeatureImp$new(model, loss = "ce")
imp$results$tmp <- "Feature Importance"

(vimp <- ggplot(data = imp$results, aes(y=feature, x=importance,
                               xmin=importance.05, xmax=importance.95)) +
  geom_point(size=2.5) +
  geom_errorbar(size=0.75,width=.1, col = "#0072B2") +
    ylab("")+
    xlab("Feature Importance (loss: ce)")+
    facet_grid(~tmp) +
    theme(text = element_text(size = 14)))




diagn <- effect_p +   vimp 
ggsave(paste0(path_out, "explanation.pdf"), diagn)

### prediction ###

library(future.apply)


lrn$parallel_predict = TRUE
plan(sequential)

pred <- lapply(predictors, function(x) {
  task_predict = as_task_regr(x, id = "prediction", task_train=task)
  y <-  mlr3spatial::predict_spatial(task_predict, lrn,chunksize = 100L)
  return(y)
})
  
x=predictors[[2]]
levs = levels(task$truth())
pfmlr = function(model, ...) {
  if(model$predict_type == "prob") {
    p = model$predict_newdata(...)$data$prob
    if(length(levs) != ncol(p)) {
      missing = setdiff(levs, colnames(p))
      pm = matrix(0, ncol = length(missing), nrow = nrow(p), dimnames = list(NULL, missing))
      p = cbind(p, pm)
      p = p[, levs]
    }
    p
  } else {
    model$predict_newdata(...)$data$response
  }
}
pred <- lapply(predictors, function(x) {
  terra::predict(x, lrn, fun=pfmlr,na.rm = TRUE)
})

pred_pres <- lapply(pred, function(x) x$presence)

pred_merge <- do.call(terra::merge, pred_pres)
writeRaster(pred_merge, "c:/0_Msc_Loek/Z_Palma/results/predictions_prob.tif")


## AOA
indices <- CreateSpacetimeFolds(trainDat,spacevar = "cl",clas="pres",k=length(unique(points$cl)))
td <- as.data.frame(task$data())
tr <- td[td$pres=="presence",]
folds <-  as.numeric(trainDat_cv[trainDat_cv$pres=="presence", ]$cl)


model_trainDI = trainDI(train = tr,
                        weight= data.frame(t(lrn$importance())),
                        variables = task$feature_names,
                        folds = f
  
)
model_trainDI$trainDI

AOA <- aoa(predictors[[1]],
                  train = tr,
                  variables = task$feature_names,
                  weight = data.frame(t(lrn$importance())),
                  folds = folds,
           trainDI = model_trainDI)


rsmp_cv <- rsmp("cv", folds = 5L)$instantiate(task)
rsmp_cv$instance[order(row_id)]$fold


#saveRDS(lrn, "c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/lrn")
#saveRDS(rr, "c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/rr")
#saveRDS(task, "c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/task")


points <- st_read("e:/sdm_nrw/points_newcv.shp")
nrw <- st_read("e:/sdm_nrw/nrw.shp")
pred_merge <- rast("c:/0_Msc_Loek/Z_Palma/results/predictions_prob.tif")



### independet validation ###

ind_p <- readxl::read_excel("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/data/Gryllus_campestris_AK_Heuschrecken_NRW.xlsx")
ind_p <- ind_p[!is.na(ind_p$`R-WERT (X-Wert)`),]
ind_p$x = ind_p$`R-WERT (X-Wert)`
ind_p$y = ind_p$`H-WERT (Y-Wert)`
ind_p = ind_p[,c("ID", "Jahr", "x", "y")]
ind_p = st_as_sf(ind_p, coords = c("x", "y"), crs = st_crs(31466)) |> 
  st_transform(st_crs(pred_merge))


ind_v <- terra::extract(pred_merge, ind_p)
bg_v <- terra::extract(pred_merge, bg)
bg_v$type <- "background"
ind_v$type <- "presence"

vd <- rbind(ind_v, bg_v)

plot(st_geometry(ind_p))
plot(pred_merge, add=TRUE)


(vd_p <- ggplot(vd) +
  geom_density(aes(x=presence, fill = type), alpha = 0.5 ) +
  xlab("suitability") +
    theme_bw() +
  theme(legend.position = c(0.8,0.8),
        text = element_text(size = 12)))
vd_p$theme <- roc_plot$theme



(roc <- autoplot(rr, type= "roc"))
roc_plot <- roc + annotate("text", x = 0.8, y = 0.2, 
                           label = paste("mean AUC =",
                                         round(rr$aggregate(measures=mlr_measures$get("classif.auc")),2))) 

library(egg)
(val_pl <-egg::ggarrange(roc_plot, vd_p, nrow = 1, labels = c("a", "b")))

ggsave(paste0(path_out, "val_pl1.pdf"), val_pl)



### maps ###

prediction_log <- log10(pred_merge)
prediction_sqrt <- sqrt(pred_merge)
prediction_cloglog <- VGAM::cloglog(pred_merge)


heatmap <- rast("c:/0_Msc_Loek/Z_Palma/results/heatmap.tif")
htmp <- terra::as.points(heatmap) |> st_as_sf()
htmb <- st_buffer(htmp, 1000) 


htmb <- htmb[htmb$heatmap >1000,]|> 
  st_union() 

htmp <- st_cast(htmb,to = "POLYGON") |> 
  st_as_sf()

htmp$area <- st_area(htmp)
x = 1*10^7
units(x) = "m^2"
htmp_big <- htmp[htmp$area>x,]

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y))
  do.call("c", lapply(x, f))
}
htmb_bb <- st_bbox_by_feature(htmp_big)

htmb_bf <- st_buffer(htmb_bb, 10000)|> 
  st_union() |> 
  st_buffer(-1000) |> 
  st_set_crs(st_crs(pred_merge))

plot(htmb_bf)


pts_suit <- terra::extract(pred_merge,vect(points[points$pres==1,]))
pts_suit <- sort(pts_suit$presence, decreasing = FALSE)
or10_th <- pts_suit[length(pts_suit)/10]
saveRDS(or10_th,"c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/or10_th")
or10_th <- readRDS("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/or10_th")

m <- c(0, or10_th, 0, or10_th, 4, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

preds_or <- terra::classify(pred_merge, rclmat)

pts_or <- terra::as.points(pred_merge)
pts_or <- pts_or[pts_or$presence >= or10_th, ]
pts_or_sf <- st_as_sf(pts_or)
pts_or_sf <- pts_or_sf[pts_or_sf$presence >= 0.8, ]

st_write(pts_or_sf, "c:/0_Msc_Loek/Z_Palma/results/pts_or.shp")
pts_or_sf <- st_read("c:/0_Msc_Loek/Z_Palma/results/pts_or.shp") |> 
  dplyr::filter(_, _$presence > 0.8)

points = pts_or_sf
cellsize = 100
bandwith = 100


st_kde <- function(points,cellsize, bandwith, extent = NULL){
  require(MASS)
  require(raster)
  require(sf)
  
  
  
  
  if(is.null(extent)){
    extent_vec <- st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- st_bbox(extent)[c(1,3,2,4)]
  }
  
  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)
  
  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize
  
  coords <- st_coordinates(points)
  matrix <- kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  kde_density = terra::raster(matrix)
}



points_kde <- st_kde(pts_or_sf,100,0.5,1)
plot(points_kde)
plot(points_sf, add = TRUE)

points$pres <- as.factor(points$pres)
library(tmap)
f <- function(x,y) {
  paste(round(10^x,3), "to <", round(10^y,3))
}

labs <- c(f(-3,-2.5), f(-2.5,-2), f(-2,-1.5), f(-1.5,-1), f(-1,-0.5),f(-0.5,0))
labs

tmap_mode("plot")
tmap_options(bg.color = "white", legend.text.color = "black")
(tm_suit <- tm_shape(prediction_log) + 
  tm_raster("values", palette = "-RdBu", 
            title = "log(suitability)", legend.hist = TRUE,
            legend.is.portrait=TRUE, legend.show = TRUE)+
    tm_graticules(lines = FALSE) +
    tm_shape(nrw) +
    tm_polygons(alpha = 0, border.col = "black", lwd=0.5) +
    tm_add_legend(
      type = "fill",
      border.col = NULL,
      labels = labs, col = rev(RColorBrewer::brewer.pal(6, "RdBu")),
      title = "suitability")) +
  tm_layout(legend.title.color = "black",
            legend.outside = TRUE, 
            legend.format = list(digits = 1),
            legend.hist.width = 0.6) 

tmap_save(tm_suit, paste0(path_out, "suit_log_transl.pdf"),
          width = 20, height = 12, units = "cm")

f = function(x) { plot(x=x, y=log10(x)) }
x=seq(0.01,1,0.01)
f(x)
10^-1

# interactive
points$presences <- as.factor("presences")
tmap_mode("view")
(tm_suit <- tm_shape(prediction$pres) + 
  tm_raster("values", palette = "-RdBu", n = 10, style = "log10", 
            title = "suitability", legend.hist = FALSE)+
  tm_graticules(lines = FALSE) +
  tm_shape(points[points$pres==1,]) +
  tm_dots("presences", palette=c(presences = "yellow"),
          shape = 1,legend.show = TRUE,
          title = "presences") +
  tm_layout(legend.title.color = "white"))
tmap_save(tm_suit, paste0(path_out, "suit_tmblack1.html"))

htmlwidgets::saveWidget(tm_suit, "suitability.html", selfcontained = T)

### maps ###


pred100 <- terra::aggregate(prediction, 5, fun = "max")
pred_ras100 <- raster::raster(pred100)
pred_gpl <- SDMSelect::gplot_data(pred_ras100)

values <- lapply(th10_ndvi, function(x) {c(x, seq(from = round(x,1)+0.1, to=1, by=0.1))})
breaks <- lapply(th10_ndvi, function(x) {c(x, seq(from = round(x,1)+0.1, to=1, by=0.1))})
labels <- lapply(th10_ndvi, function(x) {c("Schwellenwert", seq(from = round(x,1)+0.1, to=1, by=0.1))})
pal <- RColorBrewer::brewer.pal(name = "YlOrRd", n = 5)

(rsp <- ggplot() +
  geom_raster(data=pred_gpl, aes(x=x, y=y, fill = value)) +
  scale_fill_gradientn(colours = pal,
                       name = "suitability",
                       guide = guide_colorbar(reverse = FALSE,
                                              draw.ulim = TRUE,
                                              draw.llim = FALSE,
                                              frame.colour = "black"),
                       na.value="transparent") +
  ggnewscale::new_scale_fill() +
  geom_sf(data = points[points$pres == 1, ], shape = 1, colour = "white") +
  geom_sf(data = nrw, fill = NA, colour = "black") +
  coord_sf(expand = FALSE) +
  xlab("") +
  ylab("") )

ggsave(paste0(path_out, "suit.tiff"), dpi = 2600)







