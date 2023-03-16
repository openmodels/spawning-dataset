library(PBSmapping)
library(stringi)

source("code/generate/lib.R")
source("code/generate/names.R")
source("code/generate/distance.R")

custom.shppath <- "inputs/shapefiles"

spawnareas <- read.csv("inputs/Master Spawning ProCreator.csv")
spawnareas$Verdict <- gsub("^ +| +$", "", as.character(spawnareas$Verdict))
specieseez <- read.csv("inputs/specieseez.csv")

eezshp <- importShapefile("inputs/shapefiles/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp", readDBF=T)
eezshp.polydata <- attr(eezshp, "PolyData")

shorelines <- importShapefile("inputs/shapefiles/ne_50m_coastline/ne_50m_coastline-buffer025.shp")
shorelines$SID <- as.numeric(factor(paste(shorelines$PID, shorelines$SID)))
shorelines$PID <- 1

allrects <- read.csv("inputs/allrects.csv")

spawning.species <- list("Lutjanus chrysurus"="Ocyurus chrysurus")

warnpoly <- data.frame(ID=c(), issue=c(), verdict=c())

country2indexes <- function(country, polydata) {
    #if (is.na(country) || country %in% c("Elephant I."))
     if (is.na(country))
      return(c())

    if (any(polydata$Country == as.character(country)))
        return(which(polydata$Country == as.character(country)))

    ## Allow multiple countries
    if (length(grep(",", country)) > 0) {
        pids <- c()
        for (subcountry in strsplit(country, ", ")[[1]])
            pids <- c(pids, country2indexes(subcountry, polydata))
        return(pids)
    }

    country <- spawning2canonical(country)
    which(polydata$Country == as.character(country))
}

savethis <- NULL
last.row <- NULL

get.poly <- function(specie, country, localities, subeez.only=F, error.only=F, join.adm0=F) {
    specie <- as.character(specie)
    country <- as.character(country)
    localities <- as.character(localities)

    ## Find this entry in spawnareas
    if (is.na(country) || country == "") {
        row <- subset(spawnareas, (is.na(Country) | Country == "") & Localities == localities)
    } else if (is.na(localities)) {
        row <- subset(spawnareas, Country == country & is.na(Localities))
    } else {
        row <- subset(spawnareas, Country == country & Localities == localities)
    }

    if (nrow(row) > 1) {
        if (specie %in% names(spawning.species))
            findspecie <- spawning.species[[specie]]
        else
            findspecie <- specie
        ## Try to parse species exactly
        foundrows <- sapply(which(row$Verdict != "drop"), function(rr) {
            any(findspecie == stri_match_all(row$species[rr], regex=" ?([A-Za-z ]+?) \\([0-9]+%\\)")[[1]][, 2])
        })
        if (class(foundrows) == "list") {
            savethis <<- row
            row <- row[0,] # happens with all drops
            foundrows <- c()
        }
        if (!is.na(sum(foundrows)) && sum(foundrows) == 1)
            row <- row[foundrows,]
        else if (length(grep(findspecie, row$species)) > 0)
            row <- row[grep(findspecie, row$species),]
        if (nrow(row) > 1) {
            row <- row[row$Verdict != "drop",]
        }
    }
    if (nrow(row) == 0) {
        print(c("Missing valid row for", specie, country, localities))
        if (error.only)
            return("Missing valid row")
        return(data.frame())
    }
    if (nrow(row) > 1) {
        ## Check if identical - can happen if both NA and blank Country
        allmatch <- T
        for (name in c('Verdict', "New.Country", "X.1..Southwest.Coordinate", "X.1..Northeast.Coordinate", "X.2..Southwest.Coordinate", "X.2..Northeast.Coordinate", "X.3..Southwest.Coordinate", "X.3..Northeast.Coordinate")) {
            if (!all(row[1, name] == row[-1, name])) {
                allmatch <- F
                break
            }
        }

        if (allmatch) {
            row <- row[1,]
        } else {
            print(c("Multiple rows for", specie, country, localities))
            print(row)
        }
    }

    last.row <<- row
    if (nrow(row) > 1) {
        ## Very uncommon, but no way to distinguish
        polys <- data.frame()
        for (ii in 1:nrow(row)) {
            poly <- get.poly.row(specie, country, localities, row[ii,],
                                 subeez.only=subeez.only, error.only=error.only, join.adm0=join.adm0)
            if (error.only)
                polys <- rbind(polys, data.frame(ii, poly))
            else if (nrow(poly) > 0) {
                if (nrow(polys) > 0)
                    poly$PID <- poly$PID + max(polys$PID)
                polys <- rbind(polys, poly)
            }
        }
        return(joinPolys(polys, operation="UNION"))
    } else {
        return(get.poly.row(specie, country, localities, row,
                            subeez.only=subeez.only, error.only=error.only, join.adm0=join.adm0))
    }
}

get.poly.row <- function(specie, country, localities, row, subeez.only=F, error.only=F, join.adm0=F) {
    stopifnot(nrow(row) == 1)

    verdict <- as.character(row$Verdict)
    if (row$New.Country != "") {
        country <- as.character(row$New.Country)
        if (country == "NA")
            country <- ""
    }

    if (verdict == "" && (is.na(country) || country == ""))
        verdict <- "FAO sheet"

    if (verdict %in% c("drop", "Drop")) {
        if (error.only)
            return("Drop verdict")
        return(data.frame())
    }

    ## Step 1: Get the administrative clipping shape
    if (!is.na(country) && (country == "Any" || country == "any" || country == "Global")) {
        if (subeez.only) {
            if (error.only)
                return("Any country but subeez-only")
            return(data.frame())
        }
        if (!join.adm0)
            adminshp <- eezshp
    } else if (is.na(country) || country == "" || verdict == "FAO sheet") {
        eezlist <- specieseez[specieseez$region == localities & specieseez$specie == specie,]
        if (nrow(eezlist) == 0) {
            if (sum(specieseez$region == localities) > 0) {
                warnpoly <<- rbind(warnpoly, data.frame(ID=row$ID, issue="Species missing in specieseez", verdict="Any species"))
                myeezs <- unique(specieseez$eez[specieseez$region == localities])
            } else {
                warnpoly <<- rbind(warnpoly, data.frame(ID=row$ID, issue="Region missing in specieseez", verdict="drop"))
                if (error.only)
                    return("Region missing in specieseez")
                return(data.frame())
            }
        } else
            myeezs <- unique(eezlist$eez)
        shps <- subset(eezshp, PID %in% myeezs)
        if (length(unique(shps$PID)) == 1 || !join.adm0)
            adminshp <- shps
        else
            adminshp <- joinPolys(shps, operation="UNION")
        if (verdict == "FAO sheet") {
            if (subeez.only) {
                if (error.only)
                    return("FAO sheet but subeez-only")
                return(data.frame())
            }
            return(adminshp)
        }
    } else {
        pid <- country2indexes(country, eezshp.polydata)
        if (length(pid) == 0) {
            warnpoly <<- rbind(warnpoly, data.frame(ID=row$ID, issue="Cannot identify country", verdict="drop"))
            if (error.only)
                return("Cannot identify country")
            return(data.frame())
        }
        adminshp <- subset(eezshp, PID %in% pid)
        if (verdict == "EEZ") {
            if (subeez.only) {
                if (error.only)
                    return("EEZ but subeez-only")
                return(data.frame())
            }
            return(adminshp)
        }
    }

    ## Step 2: Interpret bounding box
    if (verdict %in% c('green', 'red')) {
        rect <- allrects[!is.na(allrects$country) & !is.na(allrects$localities) & allrects$country == country & allrects$localities == localities & allrects$which == verdict,]
        stopifnot(nrow(rect) == 1)
        rectshp <- data.frame(PID=1, POS=1:4, X=c(rect$west, rect$west, rect$east, rect$east),
                              Y=c(rect$south, rect$north, rect$north, rect$south))
    } else if (verdict == "Google maps") {
        rectshp <- data.frame(PID=c(), SID=c(), POS=c(), X=c(), Y=c())
        for (kk in 1:3) {
            swcoord <- as.character(row[1, paste0("X.", kk, "..Southwest.Coordinate")])
            necoord <- as.character(row[1, paste0("X.", kk, "..Northeast.Coordinate")])
            if (swcoord == "" && necoord == "")
                next
            stopifnot(swcoord != "" && necoord != "")
            swlatlon <- as.numeric(strsplit(swcoord, ", ")[[1]])
            nelatlon <- as.numeric(strsplit(necoord, ", ")[[1]])
            rectshp <- rbind(rectshp, data.frame(PID=1, SID=kk, POS=1:4, X=c(swlatlon[2], swlatlon[2], nelatlon[2], nelatlon[2]),
                                                 Y=c(swlatlon[1], nelatlon[1], nelatlon[1], swlatlon[1])))
        }
    } else if (verdict == "near-shore") {
        rectshp <- shorelines
    } else if (verdict == "native range" || (country == "Global" && localities == "Global")) {
        rectshp <- data.frame(PID=1, SID=1, POS=1:4, X=c(-180, -180, 180, 180), Y=c(-90, 90, 90, -90))
    } else if (length(grep("\\.shp", verdict)) == 1) {
        rectshp <- importShapefile(file.path(custom.shppath, verdict))
        if (all(rectshp$X > 180))
            rectshp$X <- rectshp$X - 360
    } else {
        if (verdict == "" && !subeez.only) {
            if (row$Total.Catch < 50000)
                warnpoly <<- rbind(warnpoly, data.frame(ID=NA, issue="No verdict, but < 50000", verdict="EEZ"))
            else
                warnpoly <<- rbind(warnpoly, data.frame(ID=row$ID, issue="No verdict", verdict="EEZ"))
            if (is.null(adminshp))
                adminshp <- data.frame(PID=1, SID=1, POS=1:4, X=c(-180, -180, 180, 180), Y=c(-90, 90, 90, -90))
            return(adminshp)
        }
        print(row)
        if (error.only)
            return("No verdict")
        return(data.frame())
    }

    ## Step 3: Intersect bounding box and administrative shape
    if (!is.null(adminshp)) {
        intersects <- tryCatch({
            joinPolys(adminshp, rectshp, operation="INT")
        }, error=function(e) {
            print(row)
            warnpoly <<- rbind(warnpoly, data.frame(ID=row$ID, issue="Bad polygon", verdict="drop"))
            data.frame()
        })

        if (length(unique(intersects$PID)) == 1 && length(unique(adminshp$PID)) == 1)
            intersects$PID <- adminshp$PID[1]
    } else {
        intersects <- rectshp
    }

    if (is.null(intersects)) {
        warnpoly <<- rbind(warnpoly, data.frame(ID=row$ID, issue="No intersection", verdict="drop"))
        if (error.only)
            return("No intersection")
        return(data.frame())
    }
    if (nrow(intersects) == 0 && error.only)
        return("Bad polygon")

    return(intersects)
}

last.status <- NULL

region.x.suitability <- function(species, shp, float.gridref=NULL) {
    last.status <<- NULL
    species <- as.character(species)
    filepath <- file.path("inputs/ranges", paste0(gsub(" ", "-", species), ".csv"))
    if (!file.exists(filepath)) {
        last.status <<- "Cannot find suitability data."
        return(data.frame())
    }
    tbl <- get.table(filepath)
    events <- data.frame(EID=1:nrow(tbl), X=tbl$Center.Long, Y=tbl$Center.Lat)
    found <- region.x.suitability.events(events, shp, float.gridref)
    if (is.null(found))
        return(data.frame())
    tbl[found$EID, ]
}

region.x.suitability.events <- function(events, shp, float.gridref=NULL, allowed=NULL) {
    last.status <<- NULL
    events <- as.EventData(events, projection=1)
    found <- findPolys(events, shp, maxRows=nrow(events))
    if (is.null(found) && length(unique(shp$PID)) == 1) {
        if (is.null(float.gridref)) {
	    last.status <<- "Empty locality: missing gridref"
	    return(NULL)
	}
        if (sum(calcArea(shp)$area) < 3 * .5*.5) {
            centroid <- calcCentroid(shp, rollup=1)
            if (!is.null(allowed)) {
                dists <- rep(Inf, nrow(float.gridref))
                dists[allowed] <- gcd.slc(mean(centroid$X), mean(centroid$Y), float.gridref$V1[allowed], float.gridref$V2[allowed])
            } else {
                dists <- gcd.slc(mean(centroid$X), mean(centroid$Y), float.gridref$V1, float.gridref$V2)
            }
            if (min(dists) < 100) {
                eid <- which.min(dists)
                found <- data.frame(EID=eid, PID=unique(shp$PID), X=float.gridref$V1[eid], Y=float.gridref$V2[eid])
            } else {
                last.status <<- paste0("Empty locality: ", mean(centroid$X), ", ", mean(centroid$Y), ": ", min(dists))
                return(NULL)
            }
        } else {
            last.status <<- "Empty locality: Large area"
            return(NULL)
        }
    }

    found
}
