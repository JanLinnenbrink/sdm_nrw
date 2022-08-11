library(terra)
library(sf)
library(mlr3)
library(mlr3learners)
library(mlr3spatial)
library(mlr3spatiotempcv)
library(mlr3extralearners)

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
cl <- trainDat$cl
trainDat <- trainDat |> subset(select =-cl) 

trainDat$pres <- as.factor(ifelse(trainDat$pres==1,"presence", "absence"))
trainDat$forest <- as.factor(trainDat$forest)
trainDat$cultiv <- as.factor(trainDat$cultiv)

backend <- as_data_backend(trainDat, keep_rownames = FALSE)

task <- as_task_classif_st(backend, id = "training_points", target = "pres",
                        coordinate_names = c("x","y"), coords_as_features = FALSE,
                        crs = 25832)

lrn <- lrn("classif.randomForest", 
           importance = "accuracy", 
           mtry=2L, predict_type = "prob")

lrn$train(task)
lrn$model

# cross-validation
rsmp_cv <- rsmp("custom_cv")
f <- factor(cl)
rsmp_cv$instantiate(task, f=f)
#

rcv = rsmp("spcv_coords", folds = 5)
rcv$instantiate(task)
rcv$score(msr("classif.auc"))
rr <- resample(task, lrn, rcv, store_models = TRUE)
rr$aggregate()

library(mlr3viz)
# Precision/recall graph
roc <- autoplot(rr, type = "roc")
(d <- autoplot(rr, type = "prc", bins = 30))

autoplot(task)

library(iml)
trainDat <- trainDat |> subset(select = -x) |> subset(select=-y)
trainDat <- trainDat[complete.cases(trainDat),]
x <- trainDat[which(names(trainDat) != "pres")]

model = Predictor$new(lrn$model, data = x, y = trainDat$pres)
effect = FeatureEffects$new(model)
effect$plot(features = c("lstat", "age", "rm"))
effect_p <- plot(effect)

x.interest = data.frame(x[1, ])
shapley = Shapley$new(model, x.interest = x.interest)
plot(shapley)


imp = FeatureImp$new(model, loss = "ce")
imp$results$tmp <- "Feature Importance"

(vimp <- ggplot(data = imp$results, aes(y=feature, x=importance,
                               xmin=importance.05, xmax=importance.95)) +
  geom_point(size=2.5) +
  geom_errorbar(size=0.75,width=.1, col = "#0072B2") +
    ylab("")+
    xlab("Feature Importance (loss: ce)")+
    facet_grid(~tmp))


library(ggplot2)


(diagn <- effect_p + vimp)
ggsave(paste0(path_out, "explanation.pdf"), diagn)

### prediction ###

library(future.apply)


lrn$parallel_predict = TRUE
plan(multisession, workers = 10)

pred <- lapply(predictors, function(x) {
  task_predict = as_task_regr(x, id = "prediction", task_train=task)
  y <-  mlr3spatial::predict_spatial(task_predict, lrn)
  return(y)
})
  

pred <- lapply(predictors, function(x) {
  task_predict = as_task_classif_st(x, id = "prediction")
  y <-  mlr3spatial::predict_spatial(task_predict, lrn,chunksize = 100L)
  return(y)
})
prediction <- do.call(merge, pred)
writeRaster(prediction, paste0(path_out, "prediction_classif.tif"))

points <- st_read("e:/sdm_nrw/points_newcv.shp")
nrw <- st_read("e:/sdm_nrw/nrw.shp")
prediction <- rast(paste0(path_out, "prediction.tif"))
prediction_log <- log(prediction, 10)

points$pres <- as.factor(points$pres)
library(tmap)
tmap_mode("plot")
tmap_options(bg.color = "white", legend.text.color = "black")
(tm_suit <- tm_shape(prediction_log$layer) + 
  tm_raster("layer", palette = "-RdBu", 
            title = "log(suitability)", legend.hist = TRUE,
            legend.is.portrait=TRUE)+
    tm_graticules(lines = FALSE) +
    tm_shape(nrw) +
    tm_polygons(alpha = 0, border.col = "black", lwd=0.5) +
  tm_layout(legend.title.color = "black",
            legend.outside = TRUE,
            legend.format = list(digits = 1),
            legend.hist.width = 0.6))

tmap_save(tm_suit, paste0(path_out, "suit_log_nolines.pdf"),
          width = 20, height = 12, units = "cm")

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
library(ggplot2)


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







