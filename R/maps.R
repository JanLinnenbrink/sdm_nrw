library(terra)
library(tmap)

path_out <- "C:/0_Msc_Loek/Z_Palma/results/prediction_aoa/"

pred_merge <- rast("c:/0_Msc_Loek/Z_Palma/results/predictions_prob.tif")
AOA <- rast("c:/0_Msc_Loek/Z_Palma/results/aoa/AOA_test.tif")
points <- st_read("e:/sdm_nrw/points_newcv.shp")
nrw <- st_read("e:/sdm_nrw/nrw.shp")
or10_th <- readRDS("c:/0_Msc_Loek/M7_Fernerkundung/sdm_nrw/or10_th")

prediction_log <- log10(pred_merge)

pred_merge[AOA < 1] <- 9999
pia <- pred_merge
poa <- pred_merge

pia[pia < 0] <- NA
poa[poa >= 0] <- NA 

m <- c(99999,0, -1, 0, or10_th, 0, or10_th, 4, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
preds_bin <- terra::classify(pred_merge, rclmat)

preds_bin <- as.factor(preds_bin)
levels(preds_bin) <- data.frame(value = -1:1, label = c("outside of AOA", 
                                "unsuitable", "suitable"))

f <- function(x,y) {
  paste(round(10^x,3), "to <", round(10^y,3))
}
labs <- c(f(-3,-2.5), f(-2.5,-2), f(-2,-1.5), f(-1.5,-1), f(-1,-0.5),f(-0.5,0))

## maps ##
tmap_mode("plot")
tmap_options(bg.color = "white", legend.text.color = "black")
(tm_suit <- tm_shape(pred_merge) + 
    tm_raster("values", palette = c("grey", rev(RColorBrewer::brewer.pal(5, "RdBu"))),
              style = "fixed", labels = c( "outside of AOA", "0 to <0.2",
                                           "0.2 to <0.4", "0.4 to <0.6",
                                           "0.6 to <0.8", "0.8 to 1"),
              breaks = c(9999,0,0.2,0.4,0.6,0.8,1),
              title = "suitability", legend.hist = FALSE,
              legend.is.portrait=TRUE, legend.show = TRUE) +
    tm_graticules(lines = FALSE) +
    tm_shape(nrw) +
    tm_polygons(alpha = 0, border.col = "black", lwd=0.5) +
  tm_layout(legend.title.color = "black",
            legend.outside = TRUE, 
            legend.format = list(digits = 1),
            legend.hist.width = 0.6)) 

tmap_save(tm_suit_log, paste0(path_out, "suit_aoa_log_white.pdf"),
          width = 20, height = 12, units = "cm")


(tm_suit_log <- tm_shape(prediction_log) + 
    tm_raster("values", palette = c(rev(RColorBrewer::brewer.pal(6, "RdBu")),"white"),
              style = "fixed", labels = c(labs , "outside AOA"),
              breaks = c(seq(-3,0,0.5), log10(9999)),
              title = "suitability", legend.hist = FALSE,
              legend.is.portrait=TRUE, legend.show = TRUE) +
    tm_graticules(lines = FALSE) +
    tm_shape(nrw) +
    tm_polygons(alpha = 0, border.col = "black", lwd=0.5) +
    tm_layout(legend.title.color = "black",
              legend.outside = TRUE, 
              legend.format = list(digits = 1),
              legend.hist.width = 0.6)) 

tm_bin <- tm_shape(preds_bin) + 
    tm_raster("spat_Q7tPosFq6Reguu9_5056.tif",  
              title = "suitability", legend.hist = TRUE,
              palette=c('orange', 'blue','red'),
              legend.is.portrait=TRUE, legend.show = TRUE) +
    tm_graticules(lines = FALSE) +
    tm_shape(nrw) +
    tm_polygons(alpha = 0, border.col = "black", lwd=0.5) +
    tm_layout(legend.title.color = "black",
              legend.outside = TRUE, 
              legend.format = list(digits = 1),
              legend.hist.width = 0.6)
