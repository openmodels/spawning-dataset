library(PBSmapping)
divs <- importShapefile("inputs/shapefiles/fa_/fa_", readDBF=T)
dbfs <- attr(divs, "PolyData")
fao2pid <- data.frame(area=as.character(dbfs$F_AREA), subarea=as.character(dbfs$F_SUBAREA), division=dbfs$F_DIVISION, subdivis=dbfs$F_SUBDIVIS, subunit=dbfs$F_SUBUNIT, PID=dbfs$PID)

fao2eez <- read.csv("outputs/fao2eez.csv")
fao2eez$encoded <- paste(fao2eez$area, fao2eez$country, sep='.')
species <- read.csv("inputs/Region FAO EEZ matching-DO NOT EDIT IN EXCEL.csv")

fao2eez$area <- as.character(fao2eez$area)
fao2eez$subarea <- as.character(fao2eez$subarea)

specieseez <- data.frame(region=c(), specie=c(), eez=c(), major=c())
speciespid <- data.frame(region=c(), specie=c(), PID=c(), major=c())
for (ii in 1:nrow(species)) {
    specieseezii <- data.frame(region=c(), specie=c(), eez=c(), major=c())
    speciespidii <- data.frame(region=c(), specie=c(), PID=c(), major=c())

    specie <- strsplit(as.character(species$species[ii]), ' ')[[1]]
    specie <- gsub('-', ' ', specie)
    ## if (species$region[ii] == "California Current Region") {
    ##     for (ll in 1:length(specie))
    ##         specieseezii <- rbind(specieseezii, data.frame(region=species$region[ii], specie=specie[ll], eez=c(185, 215)))
    ##     next
    ## }

    if (species$new.FAO.region[ii] == "0")
        next
    subregs <- strsplit(as.character(species$new.FAO.region[ii]), ';')[[1]]
    if (length(subregs) == 0)
        next

    extraeezs <- c()
    for (jj in 1:length(subregs)) {
        if (grepl(':', subregs[jj])) {
            parts <- strsplit(subregs[jj], ':')[[1]]
            major <- trimws(parts[1])
            minors <- paste(major, trimws(strsplit(parts[2], ',')[[1]]), sep='.')
            for (kk in 1:length(minors)) {
                ## Check for SUB - COUNTRY notation
                if (grepl('-', minors[kk])) {
                    minorparts <- trimws(strsplit(minors[kk], '-')[[1]])
                    minors[kk] <- minorparts[1]
                    extraeezs <- c(extraeezs, minorparts[2])
                }

                pids <- unique(fao2pid$PID[!is.na(fao2pid$area) & fao2pid$area == major & ((!is.na(fao2pid$subarea) & fao2pid$subarea == minors[kk]) | (!is.na(fao2pid$division) & fao2pid$division == minors[kk]) | (!is.na(fao2pid$subdivis) & fao2pid$subdivis == minors[kk]) | (!is.na(fao2pid$subunit) & fao2pid$subunit == minors[kk]))])
                if (length(pids) > 0)
                    for (ll in 1:length(specie))
                        speciespidii <- rbind(speciespidii, data.frame(region=species$region[ii], specie=specie[ll], PID=pids, major=major))

                if (minors[kk] %in% c("87.2.3", "87.2.4", "87.1.22", "87.1.23", "87.1.24", "87.1.25", "87.2.22", "87.2.23", "87.2.24", "87.3.2", "41.3.3", "21.6.F", "21.6.G", "21.6.H", "27.V.a.1", "58.5.1", "58.4.1")) # Not associated with any EEZ
                    next
                if (minors[kk] %in% c("47.A.2")) # Added since FAO region file
                    next

                eezs <- unique(fao2eez$eez[!is.na(fao2eez$eez) & !is.na(fao2eez$area) & fao2eez$area == major & ((!is.na(fao2eez$subarea) & fao2eez$subarea == minors[kk]) | (!is.na(fao2eez$division) & fao2eez$division == minors[kk]) | (!is.na(fao2eez$subdivis) & fao2eez$subdivis == minors[kk]) | (!is.na(fao2eez$subunit) & fao2eez$subunit == minors[kk]))])
                if (length(eezs) == 0)
                    eezs <- unique(fao2eez$eez[fao2eez$encoded == minors[kk]])
                if (length(eezs) == 0 && minors[kk] == "71.China")
                    eezs <- 204 # Just give it to them

                if (length(eezs) == 0) {
                    print(paste(species$region[ii], "Cannot find", major, ':', minors[kk]))
                    next
                }
                for (ll in 1:length(specie))
                    specieseezii <- rbind(specieseezii, data.frame(region=species$region[ii], specie=specie[ll], eez=eezs, major=major))
            }
        } else {
            major <- trimws(subregs[jj])

            pids <- unique(fao2pid$PID[((!is.na(fao2pid$area) & fao2pid$area == major) | (!is.na(fao2pid$subarea) & fao2pid$subarea == major))])
            if (length(pids) > 0)
                for (ll in 1:length(specie))
                    speciespidii <- rbind(speciespidii, data.frame(region=species$region[ii], specie=specie[ll], PID=pids, major=major))

            eezs <- unique(fao2eez$eez[!is.na(fao2eez$eez) & ((!is.na(fao2eez$area) & fao2eez$area == major) | (!is.na(fao2eez$subarea) & fao2eez$subarea == major))])
            if (length(eezs) == 0) {
                print(paste(species$region[ii], "Cannot find", major))
                next
            }
            for (ll in 1:length(specie))
                specieseezii <- rbind(specieseezii, data.frame(region=species$region[ii], specie=specie[ll], eez=eezs, major=major))
        }
    }

    if (nchar(trimws(as.character(species$EEZ[ii]))) > 0 || length(extraeezs) > 0) {
        countries <- c(trimws(strsplit(as.character(species$EEZ[ii]), ",")[[1]]), extraeezs)
        tokeeps <- data.frame()
        for (country in countries) {
            if (country == "Juan Fernandes Is.")
                country <- "Chile"
            if (country == "Curacao")
                country <- "Cura\xe7ao"
            if (country == "Congo Republic")
                country <- "R\xe9publique du Congo"
            myeezs <- unique(fao2eez$eez[fao2eez$country == country])
            if (length(myeezs) == 0)
                myeezs <- unique(fao2eez$eez[fao2eez$sovereign == country])

            if (length(myeezs) == 0) {
                print(paste("Unknown country", ii, country))
                next
            }
            mymajors <- unique(specieseezii$major[specieseezii$eez %in% myeezs])
            if (length(mymajors) == 0) {
                myeezs <- unique(fao2eez$eez[fao2eez$sovereign == country])
                mymajors <- unique(specieseezii$major[specieseezii$eez %in% myeezs])
            }

            if (length(mymajors) == 0) {
                print(paste("No available majors for", ii, country))
                next
            }
            if (length(mymajors) > 1) {
                print(paste("Multiple majors for", ii, country))
                next
            }

            tokeeps <- rbind(tokeeps, data.frame(region=species$region[ii], eez=myeezs, major=mymajors))
        }

        if (nrow(tokeeps) == 0) {
            print(paste("Nothing to keep for", ii))
        } else {
            for (ll in 1:length(specie)) {
                ## Remove rows with these majors then add tokeeps
                specieseezii <- specieseezii[specieseezii$specie != specie[ll] | !(specieseezii$major %in% unique(tokeeps$major)),]
                specieseezii <- rbind(specieseezii, cbind(tokeeps, data.frame(specie=specie[ll])))
            }
        }
    }

    specieseez <- rbind(specieseez, specieseezii)
    speciespid <- rbind(speciespid, speciespidii)
}

specieseez <- specieseez[!duplicated(specieseez),]
speciespid <- speciespid[!duplicated(speciespid),]

write.csv(specieseez, "outputs/specieseez.csv", row.names=F)
write.csv(speciespid, "outputs/speciespid.csv", row.names=F)
