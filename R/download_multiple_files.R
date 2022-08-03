library(R.utils)
library(raster)


loadmras <- function(path_wd, path_csv, url, format, out_name) {
  
  kachelname <- read.csv(path_csv, sep=";")$Kachelname
  filenames <- paste0(url, kachelname, format)
  
  message("Downloading files...")
  
  for (i in seq_along(filenames)) {
    download.file(filenames[[i]], paste0(path_wd, kachelname[[i]], format))
  }
  
  if(format == ".xyz.gz"){
    
    dir.create(paste0(path_wd, "unzipped"))
    path_z <- paste0(path_wd, "unzipped/")
    
    message("Unzipping...")
    
    l <- list.files(path_wd)
    for (i in seq_along(l)) {
      gunzip(paste0(path_wd,l[[i]], path_z, kachelname[[i]], ".xyz"))
    }
    
    xf <- list.files(path_z)
    
    message("Converting .xyz to raster...")
    
    ras <- list()
    for (i in seq_along(xf)) {
      ras[[i]] <- raster(paste0(path_z,xf[i]))
    }
    
    message("Merging raster layers...")
    
    ras_merged <- do.call(merge, ras)
    
    saveRDS(ras_merged, out_name, ".RDS")
  }
  
  
  if(format != ".xyz.gz") {
    xf <- list.files(path_wd)
    
    message("Converting .xyz to raster...")
    
    ras <- list()
    for (i in seq_along(xf)) {
      ras[[i]] <- raster(paste0(path_wd,xf[i]))
    }
    
    message("Merging raster layers...")
    
    ras_merged <- do.call(merge, ras)
    
    saveRDS(ras_merged, "dgm.RDS")
  }
  
}

path_wd <- "C:/0_Msc_Loek/M7_Fernerkundung/"
path_csv <- "C:/0_Msc_Loek/M7_Fernerkundung/dgm1_xyz.csv"
url <- "https://www.opengeodata.nrw.de/produkte/geobasis/hm/dgm1_xyz/dgm1_xyz/"
format <- ".xyz.gz"
out_name <- "dgm"


kachelname <- read.csv(path_csv, sep=";")$Kachelname
url_tf <- paste0(url, kachelname, format)
writeLines(url_tf, paste0(path_wd,"url_dgm.txt"))


# PowerShell: wget -i url_dgm.txt

path_out <- "D:/dgm/"
ras <- lapply(paste0(path_out, list.files(path_out)), raster)
ras_merged <- do.call(merge, ras)


saveRDS(ras_merged, "dgm.RDS")