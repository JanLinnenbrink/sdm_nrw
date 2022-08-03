library(CAST)
library(virtualspecies)
library(caret)
library(raster)
library(sp)
library(sf)
library(viridis)
library(latticeExtra)
library(gridExtra)
library(glmnet)
library(maxnet)

predictors <- stack(system.file("extdata","bioclim.grd",package="CAST"))
response <- generateSpFromPCA(predictors,
                              means = c(3,1),sds = c(2,2), plot=F)$suitab.raster


mask <- predictors[[1]]
values(mask)[!is.na(values(mask))] <- 1
mask <- rasterToPolygons(mask)

# Generate Clustered Training Samples
csample <- function(x,n,nclusters,maxdist,seed){
  set.seed(seed)
  cpoints <- sp::spsample(x, n = nclusters, type="random")
  result <- cpoints
  result$clstrID <- 1:length(cpoints)
  for (i in 1:length(cpoints)){
    ext <- rgeos::gBuffer(cpoints[i,], width = maxdist)
    newsamples <- sp::spsample(ext, n = (n-nclusters)/nclusters, 
                               type="random")
    newsamples$clstrID <- rep(i,length(newsamples))
    result <- rbind(result,newsamples)
    
  }
  result$ID <- 1:nrow(result)
  return(result)
}


samplepoints <- st_as_sf(csample(mask,75,15,maxdist=0.20,seed=15))
background <- st_as_sf(sampleRegular(predictors[[1]], size=1000,sp=TRUE))

df <- data.frame(pres = rep(1,nrow(samplepoints)))
samplepoints <- st_sf(df, st_geometry(samplepoints))
st_geometry(samplepoints) <- "geom"

df <- data.frame(pres = rep(0,nrow(background)))
background <- st_sf(df, st_geometry(background))
st_geometry(background) <- "geom"

points <- rbind(samplepoints,background)

trainDat <- extract(predictors,samplepoints,df=TRUE)%>% 
  .[complete.cases(.),]
trainDatbg <- extract(predictors,background,df=TRUE) %>% 
  .[complete.cases(.),]

trainDat$pres <- 1
trainDatbg$pres <- 0

trainDat <- rbind(trainDat, trainDatbg)

trainDat$weight = trainDat$pres + (1-trainDat$pres) * 10000

p = trainDat[,"pres"]
data = trainDat[,c("bio2", "bio5", "bio10", "bio13", "bio14", "bio19")]
form = maxnet.formula(p, data)

mm <- model.matrix(form,data)

regmult = 1
regfun=maxnet.default.regularization
reg <- regfun(p,mm)*regmult
weights <- p + (1 - p) * 100
lambda = 10^(seq(4, 0, length.out=200)) * sum(reg)/length(reg)*sum(p)/sum(weights)
mod.glmnet1 <- glmnet::glmnet(x=mm,y=as.factor(p),family="binomial",
                              penalty.factor = reg, lambda=lambda, weights = weights)

#
simplerform = maxnet.formula(p, data, classes = "lqp")
mm <- model.matrix(simplerform,data)
reg <- regfun(p,mm) * regmult
lambda = 10^(seq(4, 0, length.out=200)) * sum(reg)/length(reg)*sum(p)/sum(weights)
mod.glmnet2.cv <- glmnet::cv.glmnet(x=mm,y=as.factor(p),family="binomial",
                                    penalty.factor = reg, lambda=lambda, weights = weights)


plot(mod.glmnet1)

prd <- raster::predict(object=predictors, model=mod.glmnet1)



# cluster -----------------------------------------------------------------
library(terra)
set.seed(1234)

v<- 5
coords <- sf::st_centroid(sf::st_geometry(samplepoints))
dists <- as.dist(sf::st_distance(coords))
km <- kmeans(dists, centers = v)
samplepoints$cl <- km$cluster

background <- st_as_sf(sampleRegular(predictors[[1]], size=1000,sp=TRUE))

samplennb <- GeoInterpolation::doNearestNeighbor(cl~1,samplepoints,background)

df <- data.frame(pres = rep(1,nrow(samplepoints)))
samplepoints_g <- st_sf(df, st_geometry(samplepoints))
samplepoints_g$cl <- samplepoints$cl
st_geometry(samplepoints_g) <- "geom"

df <- data.frame(pres = rep(0,nrow(background)))
background_g <- st_sf(df, st_geometry(background))
background_g$cl <- samplennb$var1.pred
st_geometry(background_g) <- "geom"

points <- rbind(samplepoints,background_g)

trainDat <- data.frame(cl=points$cl, pres = points$pres,
                       extract(predictors, points, df=TRUE))%>% 
  .[complete.cases(.),]


##cast
indices <- CreateSpacetimeFolds(trainDat,spacevar = "cl",clas="pres",k=5)
ctrl <- trainControl(method="cv",index=indices)

model_ffs <- ffs(predictors = trainDat[,names(trainDat) %in% names(predictors)],
    response = trainDat$pres,
    method="glmnet",
    trControl=trainControl(method="cv",index=indices$index),
    form = form,
    family="binomial",
    penalty.factor = reg, lambda=lambda, weights = weights)

suit_map <- predict(predictors,model_ffs)
plot(suit_map)
plot(samplepoints$geom, add=TRUE)

preds <- model_ffs$pred[model_ffs$pred$mtry==model_ffs$bestTune$mtry,]
table(preds$pred,preds$obs)

#method = 'glmnet'
model_gl <- cv.glmnet(x=mm,y=as.factor(p),family="binomial",
                      penalty.factor = reg, lambda=lambda, weights = weights,
                      foldid = trainDat$cl)
maxnet(p,)
maxnet::response.plot()
coef(model_gl, s = "lambda.min")

pred <- myspatial::predict_glmnet_raster(predictors, model_gl)
plot(rocs[[best]], type = "l")
plot(model_gl)

new_data <- as.data.frame(predictors) %>% 
  .[complete.cases(.),]
nx <- model.matrix(form,new_data)

prd <-predict(model_gl,nx)

rocs <- roc.glmnet(model_gl,newy=)
library(caret)







# Split dataframe into training & testing sets
inTrain <- createDataPartition(trainDat$pres, p = .75, list = FALSE)
Train <- trainDat[ inTrain, ] # Training dataset for all model development
Test <- trainDat[ -inTrain, ] # Final sample for model validation

# Run logistic regression , using only specified predictor variables 
logCV <- cv.glmnet(x = data.matrix(Train[, c(2:7)]), y = Train[,"pres"],
                   family = 'binomial', type.measure = 'auc')

# Test model over final test set, using specified predictor variables
# Create field in dataset that contains predicted values
Test$prob <- predict(logCV,type="response", newx = data.matrix(Test[,c(2:7) ]), s = 'lambda.min')





trainDat$response <- extract (response,samplepoints)
trainDat <- merge(trainDat,samplepoints,by.x="ID",by.y="ID")
trainDat <- trainDat[complete.cases(trainDat),]



library(glmnet)





