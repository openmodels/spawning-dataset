library("raster")
library("ncdf4")
library("rgdal")
library("rgeos")
library("ggplot2")
library("sf")

fp <- "/Users/ram114/Dropbox/Spawning ProCreator/shapefiles/"
fl <- c("cont_shelf","cont_slope")
for (i in c(1:2)){
  in_fn <- paste(fp,fl[i],".nc",sep="")
  out_fn <- paste(fp,fl[i],".shp",sep="")
  ncfile <- nc_open(in_fn)
  print(ncfile)
  varn <- toupper(sub("\\-.*", "", fl[i]))
  this_brick <- brick(in_fn,varname=varn)
  this_raster <- raster(this_brick,layer=1)
  this_poly <- rasterToPolygons(this_raster,dissolve=TRUE)
  writeOGR(this_poly,dsn=out_fn,layer=tolower(varn),driver="ESRI Shapefile", overwrite_layer=T)
}