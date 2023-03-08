library("raster")
library("ncdf4")
library("rgdal")
library("rgeos")
library("ggplot2")
library("sf")

fp <- "/Users/nandini/Dropbox/Spawning ProCreator/shapefiles/"
fl <- c("chile-46","canada-477","norway-922","cuba-1101","newzealand-1728","jamaica-1839")
for (i in c(1:6)){
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

#Western Atlantic needs to be done separately
#Get continental slope file (this uses the Encyclopaedia Britannica definition of 100-3200 m depth):
ncfile <- nc_open(paste(fp,"cont_slope.nc",sep=""))
#Make polygon
cs_brick <- brick(paste(fp,"cont_slope.nc",sep=""),varname="CONT_SLOPE")
cs_raster <- raster(cs_brick,layer=1)
cs_poly <- rasterToPolygons(cs_raster,dissolve=TRUE)
#Read shapefile for FAO regions:
faoshp <- st_read("/Users/nandini/fishnets/shapefiles/fa_/fa_.shp")
#Get regions that are in 21,31,41 (western atlantic)
westatl_regions_notvalid <- faoshp[faoshp$F_AREA == "21" | faoshp$F_AREA == "31" | faoshp$F_AREA == "41",]
#There's a loop in this outline that needs to be fixed
westatl_regions <- st_make_valid(westatl_regions_notvalid)
westatl_full <- st_union(westatl_regions)
#Take the intersection of the continental slope and these FAO regions
westatl_contslope <- st_intersection(st_make_valid(st_as_sf(cs_poly)),westatl_full)
#Write to file
st_write(westatl_contslope,dsn="/Users/nandini/Dropbox/Spawning ProCreator/shapefiles/westatl-1730.shp",layer="westatl",driver="ESRI Shapefile", overwrite_layer=T)
