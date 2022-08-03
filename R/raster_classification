# Takes an input raster x and a number of classes n
# to classify a raster dataset based on a method m
# 1 = quantiles, 2 = equal intervals, 3 = natural jenks
# if m = 1, then max 5 classes are allowed (n <= 5)

recl_ext <- function(x, n, m=1){
  
  rq <- list("raster", "BAMMtools")
  lapply(rq, require, character.only = TRUE)
  
  if(m == 1) {
    q <- as.data.frame(raster::quantile(x))
    if(n == 2) {
      m <- matrix(c(0, q[3,1], 1, q[3,1], q[5,1], 2),
                  ncol = 3, byrow = TRUE)
    }
    if(n == 3) {
      m <- matrix(c(0, q[3,1], 1, q[3,1], q[4,1], 2,
                    q[4,1], q[5,1], 3), ncol = 3, byrow = TRUE)
    }
    if(n == 4) {
      m <- matrix(c(0, q[2,1], 1, q[2,1], q[3,1], 2,
                    q[3,1], q[4,1], 3, q[4,1], q[5,1], 4),
                  ncol = 3, byrow = TRUE)
    }
    if(n == 5) {
      m <- matrix(c(0, q[1,1], 1, q[1,1], q[2,1], 2,
                    q[2,1], q[3,1], 3, q[3,1], q[4,1], 4,
                    q[4,1], q[5,1], 5), ncol = 3, byrow = TRUE)
    }
    if(n > 5) {
      stop("number of classes too high.
             Choose another classification method or
             a lower number of classes (<5)")
    }    
  }
  
  if(m==2) {
    r <- raster::cut(x, seq(from=0, to=maxValue(x), by=maxValue(x)/n))
  }
  
  if(m==3){
    b <- getJenksBreaks(raster::getValues(x), n, subset = NULL)
    c <- list()
    for(n in 1:n) {
      c[[n]] <- c(b[n-1], b[n], n)
    }
    
    c[[1]][3] <- c[[1]][2]
    c[[1]][2] <- c[[1]][1]
    c[[1]][1] <- 0
    
    m <- matrix(unlist(c), ncol = 3, byrow = TRUE)
  }
  
  r <- reclassify(x, m)
  return(r)
}
