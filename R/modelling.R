library(flexsdm)
library(caret)
library(CAST)
library()
remotes::install_github("rvalavi/blockCV", dependencies = TRUE, force=TRUE)


# loading the package
library(randomForest)

# convert the response to factor for producing class relative likelihood
training$occ <- as.factor(training$occ)

prNum <- as.numeric(table(training$occ)["1"]) # number of presences

# the sample size in each class; the same as presence number
smpsize <- c("0" = prNum, "1" = prNum)


rf_downsampled <- randomForest(formula = occ ~., 
                              data = training, 
                              ntree = 1000, 
                              sampsize = smpsize,
                              replace = TRUE)

rfpred <- predict(rf_downsampled, testing_env, type = "prob")

plot(rf_downsample, main = "RF down-sampled")
