library(R.utils)
library(raster)

kachelname <- read.csv("dgm1_xyz.csv", sep=";")$Kachelname

filenames <- paste0("https://www.opengeodata.nrw.de/produkte/geobasis/hm/dgm1_xyz/dgm1_xyz/",kachelname, ".xyz.gz")


message("Downloading files...")

for (i in seq_along(filenames)) {
  download.file(filenames[[i]], paste0("C:/0_Msc_Loek/M7_Fernerkundung/dgm/", kachelname[[i]], ".xyz.gz"))
}

p <- "C:/0_Msc_Loek/M7_Fernerkundung/dgm/"
f <- list.files("C:/0_Msc_Loek/M7_Fernerkundung/dgm/")
dir.create(paste0(p, "unzipped/"))


message("Unzipping downloaded files...")

for (i in seq_along(f)) {
  gunzip(paste0(p,f[[i]]), paste0("C:/0_Msc_Loek/M7_Fernerkundung/dgm/unzipped/", kachelname[[i]], ".xyz"))
}

xf <- list.files("C:/0_Msc_Loek/M7_Fernerkundung/dgm/unzipped/")

message("Converting .xyz to raster...")

ras <- list()
for (i in seq_along(xf)) {
  ras[[i]] <- raster(paste0("C:/0_Msc_Loek/M7_Fernerkundung/dgm/unzipped/",xf[i]))
}


message("Merging raster layers...")

ras_merged <- do.call(merge, ras)

saveRDS(ras_merged, "dgm.RDS")
