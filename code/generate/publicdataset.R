require(sp)
library(rgeos)

source("code/generate/read.R")

spawning <- read.csv("inputs/spawning-records.csv")
spawning <- spawning[!duplicated(spawning),]

suitdir <- "inputs/ranges"

allpid <- 0
allshp <- data.frame()
polydata <- data.frame()

method.messages <- list("EEZ"="Exclusive economic zone", "red"="ArcGIS geocoding", "green"="GeoNames geocoding",
                        "FAO sheet"="Multiple EEZs by FAO region", "Google maps"="Hand-geocoded",
                        "near-shore"="Near-shore region", "drop"="Drop", "chile-46.shp"="Bathymetry-based polygon",
                        "canada-477.shp"="Bathymetry-based polygon", "native range"="Entire native range",
                        "norway-922.shp"="Bathymetry-based polygon", "cuba-1101.shp"="Bathymetry-based polygon",
                        "Drop"="Drop", "newzealand-1728.shp"="Bathymetry-based polygon",
                        "jamaica-1839.shp"="Bathymetry-based polygon")

for (ii in 1:nrow(spawning)) {
    print(ii)
    shp <- get.poly(spawning$species[ii], spawning$country[ii], spawning$localities[ii])
    if (nrow(shp) == 0) {
        errmsg <- get.poly(spawning$species[ii], spawning$country[ii], spawning$localities[ii], error.only=T)
	next
    }

    method <- ifelse(nrow(last.row) > 1, "Multiple entries", method.messages[[last.row$Verdict]])
    notes <- ""

    ## Intersect with suitability
    suitmap <- region.x.suitability(spawning$species[ii], shp)
    if (nrow(suitmap) == 0 && !is.null(last.status)) {
        if (last.status == "Empty locality: missing gridref")
            notes <- "No available suitability"
        else {
            errmsg <- last.status
            last.status <- NULL
            next
        }
    }

    if (nrow(suitmap) != 0) {
        gridpoly <- data.frame()
        for (jj in 1:nrow(suitmap)) {
            gridpoly <- rbind(gridpoly, data.frame(PID=jj, SID=1, POS=1:4,
                                   X=c(suitmap$Center.Long[jj] - 0.25, suitmap$Center.Long[jj] + 0.25,
				       suitmap$Center.Long[jj] + 0.25, suitmap$Center.Long[jj] - 0.25),
                                   Y=c(suitmap$Center.Lat[jj] + 0.25, suitmap$Center.Lat[jj] + 0.25,
				       suitmap$Center.Lat[jj] - 0.25, suitmap$Center.Lat[jj] - 0.25)))
        }

        if (length(unique(gridpoly$PID)) > 1)
            gridpoly <- joinPolys(gridpoly, operation='UNION')
	shp <- joinPolys(shp, gridpoly, operation='INT')
    }

    if (length(unique(shp$PID)) > 1)
        shp <- combinePolys(shp)

    allpid <- allpid + 1
    shp$PID <- allpid
    allshp <- rbind(allshp, shp)
    polydata <- rbind(polydata, cbind(PID=allpid, spawning[ii, ], method, notes))
}

shapes <- list()
for (PID in unique(allshp$PID)) {
    print(PID)
    shp <- allshp[allshp$PID == PID,]
    polygons <- list()
    for (SID in unique(shp$SID)) {
        sshp <- shp[shp$SID == SID,]
	hole <- diff(sshp$POS[1:2]) < 0
        polygons[[SID]] <- Polygon(coords=as.matrix(rbind(sshp[, c('X', 'Y')], data.frame(X=sshp$X[1], Y=sshp$Y[1]))), hole=hole)
    }
    shapes[[PID]] <- Polygons(polygons, ID=PID)
}

spolys <- SpatialPolygons(shapes)
library(cleangeo)
sploys <- clgeo_Clean(spolys)

row.names(polydata) <- polydata$PID

validpid <- c()
for (PID in 1:length(slot(spolys, 'polygons'))) {
    print(PID)
    subspolys = spolys
    slot(subspolys, 'polygons') <- slot(sploys, 'polygons')[c(validpid, PID)]

    spdf <- SpatialPolygonsDataFrame(subspolys, polydata[c(validpid, PID),])
    success <- tryCatch({
        writeOGR(spdf, layer="GO-FISH", "outputs", driver="ESRI Shapefile", overwrite_layer=T)
	T
    }, error=function(e) {
        print(e)
        F
    })
    if (success)
      validpid <- c(validpid, PID)
}

spdf <- SpatialPolygonsDataFrame(subspolys, polydata[validpid, -1])
writeOGR(spdf, layer="GO-FISH", "outputs", driver="ESRI Shapefile", overwrite_layer=T)
write.csv(polydata, "outputs/GO-FISH.csv", row.names=F)
