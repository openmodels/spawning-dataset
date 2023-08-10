setwd("~/Dropbox/Climate Change Fish Nets")
datapath <- "~/research/fishnets/"

require(sp)
library(rgeos)

allow.highseas <- T

source("code/spawning/read.R")
##source("code/ranges/lib.R")

spawning <- read.csv("code/spawning-records.csv")
spawning <- spawning[!duplicated(spawning),]

suitdir <- file.path(datapath, "ranges/current")

allpid <- 0
allshp <- data.frame()
polydata <- data.frame()
errors <- data.frame()

method.messages <- list("EEZ"="Exclusive economic zone", "red"="ArcGIS geocoding", "green"="GeoNames geocoding",
                        "FAO sheet"="Multiple EEZs by FAO region", "Google maps"="Hand-geocoded",
                        "near-shore"="Near-shore region", "drop"="Drop", "chile-46.shp"="Bathymetry-based polygon",
                        "canada-477.shp"="Bathymetry-based polygon", "native range"="Entire native range",
                        "norway-922.shp"="Bathymetry-based polygon", "cuba-1101.shp"="Bathymetry-based polygon",
                        "Drop"="Drop", "newzealand-1728.shp"="Bathymetry-based polygon",
                        "jamaica-1839.shp"="Bathymetry-based polygon")

get.method <- function(verdict) {
    if (verdict %in% names(method.messages))
        return(method.messages[[verdict]])

    if (grepl("\\.shp", verdict)) {
        if (grepl("^mr-", verdict))
            return("Marine regions feature")
        else if (grepl("shelf|seamount|slope|ridge", verdict))
            return("Marine geographic feature")
        else
            return(verdict) # return("Bathymetry-based polygon")
    }
    verdict
}

for (ii in 1:nrow(spawning)) {
    print(ii)
    last.status <<- NULL
    shp <- tryCatch({
        shp <- get.poly(spawning$species[ii], spawning$country[ii], spawning$localities[ii], allow.highseas=allow.highseas)
        if (nrow(shp) == 0)
            last.status <<- get.poly(spawning$species[ii], spawning$country[ii], spawning$localities[ii],
                                     error.only=T, allow.highseas=allow.highseas)
        shp
    }, error=function(err) {
        last.status <<- paste("Unknown error:", as.character(err))
        data.frame()
    })

    if (!is.null(last.status)) {
        errors <- rbind(errors, data.frame(species=spawning$species[ii], country=spawning$country[ii], locality=spawning$localities[ii], message=last.status))
        next
    }

    methods <- sapply(last.row$Verdict, get.method)
    if (length(unique(methods)) > 1)
        method <- "Multiple methods"
    else
        method <- unique(methods)
    notes <- ""

    ## Intersect with suitability
    last.status <<- NULL
    suitmap <- region.x.suitability(spawning$species[ii], shp)
    if (nrow(suitmap) == 0 && !is.null(last.status)) {
        if (last.status == "Empty locality: missing gridref")
            if (sum(calcArea(shp, rollup=1)) > 3 * .5*.5) {
                errors <- rbind(errors, data.frame(species=spawning$species[ii], country=spawning$country[ii], locality=spawning$localities[ii], message="Empty locality: Large area"))
                next
            }
            notes <- "No available suitability"
        else {
            errmsg <- last.status
            errors <- rbind(errors, data.frame(species=spawning$species[ii], country=spawning$country[ii], locality=spawning$localities[ii], message=errmsg))
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
    if (!("SID" %in% names(shp)))
        shp$SID <- 1
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

shpname <- ifelse(allow.highseas, "GO-FISH-hs", "GO-FISH")

spolys <- SpatialPolygons(shapes)

## library(cleangeo)
## sploys <- clgeo_Clean(spolys)
library(rgdal)

row.names(polydata) <- polydata$PID

validpid <- c()
lastpid <- 0
## Faster approach to find invalid polygons
while (lastpid < length(slot(spolys, 'polygons'))) {
    testpid <- min(lastpid + 400, ceiling((lastpid + length(slot(spolys, 'polygons'))) / 2))
    failpid <- NA
    while (T) {
        print(paste(lastpid, testpid, sep=" - "))

        subspolys = spolys
        slot(subspolys, 'polygons') <- slot(spolys, 'polygons')[c(validpid, (lastpid+1):testpid)]

        spdf <- SpatialPolygonsDataFrame(subspolys, polydata[c(validpid, (lastpid+1):testpid),])
        success <- tryCatch({
            writeOGR(spdf, layer=shpname, "~/Dropbox/Spawning ProCreator/dataset", driver="ESRI Shapefile", overwrite_layer=T)
            T
        }, error=function(e) {
            print(e)
            F
        })
        if (success && is.na(failpid)) {
            ## Success on first try: take them all
            validpid <- c(validpid, (lastpid+1):testpid)
            lastpid <- testpid
            break
        } else if (success) {
            ## Success after narrowing: take it but see what more can take
            validpid <- c(validpid, (lastpid+1):testpid)
            lastpid <- testpid
            testpid <- ceiling((lastpid + failpid) / 2)
        } else if (testpid == lastpid + 1) {
            ## Failed and no room left to explore
            lastpid <- testpid
            break
        } else {
            failpid <- testpid
            testpid <- ceiling((lastpid + testpid) / 2)
        }
    }
}

## errors <- rbind(errors, data.frame(species=polydata$species[-validpid], country=polydata$country[-validpid],
##                                    locality=polydata$localities[-validpid], message="Invalid polygon"))
## spdf <- SpatialPolygonsDataFrame(subspolys, polydata[validpid, -1])

## Fix invalids
invalids <- (min(validpid):max(validpid))[-validpid]
for (invalid in invalids) {
    print(invalid)
    ## plotMap(subset(allshp, PID == invalid))
    spolys <- SpatialPolygons(shapes[invalid])
    fixedpoly <- gBuffer(spolys, width=0)
    slot(slot(fixedpoly, 'polygons')[[1]], 'ID') <- as.character(invalid)
    ## spdf <- SpatialPolygonsDataFrame(fixedpoly, polydata[invalid,])
    ## writeOGR(spdf, layer="test", "~/Dropbox/Spawning ProCreator/dataset", driver="ESRI Shapefile", overwrite_layer=T)

    shapes[[invalid]] <- slot(fixedpoly, 'polygons')[[1]]
}
spolys <- SpatialPolygons(shapes)
spdf <- SpatialPolygonsDataFrame(spolys, polydata)

writeOGR(spdf, layer=shpname, "~/Dropbox/Spawning ProCreator/dataset", driver="ESRI Shapefile", overwrite_layer=T)
write.csv(polydata, paste0("~/Dropbox/Spawning ProCreator/dataset/", shpname, ".csv"), row.names=F)
write.csv(errors, paste0("~/Dropbox/Spawning ProCreator/dataset/", shpname, "-errors.csv"), row.names=F)

## Statistics

length(unique(polydata$species[validpid]))

shpname <- ifelse(allow.highseas, "GO-FISH-hs", "GO-FISH")
errors <- read.csv(paste0("~/Dropbox/Spawning ProCreator/dataset/", shpname, "-errors.csv"))

subset(errors, message == "No verdict")
