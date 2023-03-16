library(PBSmapping)
divs <- importShapefile("inputs/shapefiles/fa_/fa_", readDBF=T)
dbfs <- attr(divs, "PolyData")
proj.abbr <- attr(divs, "projection")

eezs <- importShapefile("inputs/shapefiles/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014", readDBF=T)
eezs.info <- attr(eezs, "PolyData")

events <- data.frame(EID=1:nrow(eezs), X=eezs$X, Y=eezs$Y)
events <- as.EventData(events, projection=proj.abbr)

overlaps <- findPolys(events, divs, maxRows=1e7)

results <- data.frame(area=c(), subarea=c(), division=c(), subdivis=c(), subunit=c(), eez=c(), iso=c(), country=c(), sovereign=c())
for (pid in 1:nrow(dbfs)) {
    withins <- unique(eezs$PID[overlaps$EID[overlaps$PID == pid]])
    if (length(withins) == 0)
        next

    ## Drop some intersections
    if (!is.na(dbfs$F_AREA[pid])) {
        if (dbfs$F_AREA[pid] == 87)
            withins <- withins[!(eezs.info$Country[withins] %in% c("Costa Rica", "Panama"))]
        if (dbfs$F_AREA[pid] == 77)
            withins <- withins[!(eezs.info$Country[withins] %in% c("Colombia"))]
    }

    results <- rbind(results, data.frame(area=dbfs$F_AREA[pid], subarea=dbfs$F_SUBAREA[pid], division=dbfs$F_DIVISION[pid], subdivis=dbfs$F_SUBDIVIS[pid], subunit=dbfs$F_SUBUNIT[pid], eez=withins, ios=eezs.info$ISO_3digit[withins], country=eezs.info$Country[withins], sovereign=eezs.info$Sovereign[withins]))
}

write.csv(results, "outputs/fao2eez.csv", row.names=F)
