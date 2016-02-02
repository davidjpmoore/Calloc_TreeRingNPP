library(raster)

# setwd("~/PhD/CRUNCEP_monthly") # Set the directory where the met is
dir.met <- "~/PhD/CRUNCEP_monthly"
files.tair <- dir(file.path(dir.met, "tair"), ".nc")
met.tair <- data.frame(array(dim=c(110, 12)))

  valles.pt <- data.frame(lat=38.74, lon=-92.20)
  valles.pt <- SpatialPointsDataFrame(coords=valles.pt[,c("lon", "lat")], valles.pt, proj4string=CRS("+proj=longlat"))

for(i in 1:110){
  
  # get list of .nc files for that met var
  #setwd("C:/Users/babst/Desktop/data/data_climate/cruncep_data/tair")
  # /tair")
  # nc.tair <- nc_open(file.path(dir.met, "tair", files.tair[i]))
  # summary(nc.tair$var)

  tair.raster <- stack(file.path(dir.met, "tair", files.tair[i]))
  # tair.raster
  # plot(tair.raster[[1:5]])
  # plot(tair.raster[[1]]); plot(valles.pt, add=T, cex=5)
  
  met.tair[i,] <- t(extract(tair.raster, valles.pt, method="simple"))
  row.names(met.tair)[i] <- substr(names(tair.raster)[1], 2,5)
 }
