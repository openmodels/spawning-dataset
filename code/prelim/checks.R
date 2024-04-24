spawnareas <- read.csv("inputs/Master Spawning ProCreator.csv")

## Drop obsolete columns, if exist
spawnareas <- spawnareas[, -which(names(spawnareas) %in% c('Map.Tool', 'Cumul.Catch', 'Additional'))]

stopifnot(all(c("ID", "Country", "Localities", "species", "Verdict", "New.Country", "X.1..Southwest.Coordinate",
                "X.1..Northeast.Coordinate", "X.2..Southwest.Coordinate", "X.2..Northeast.Coordinate", "X.3..Southwest.Coordinate",
                "X.3..Northeast.Coordinate", "Latitude.Range") %in% names(spawnareas)))

for (ii in 1:nrow(spawnareas)) {
    if (spawnareas$Verdict[ii] %in% 'EEZ') {
        ## Check if Country is not NA
        if (is.na(spawnareas$Country[ii]))
            print(paste("Verdict EEZ on row", ii, "with Country", spawnareas$Country[ii]))
    } else if (spawnareas$Verdict[ii] %in% c("red", "green", "near-shore", "drop", "native range")) {
        ## Pass
    } else if (spawnareas$Verdict[ii] == "Google maps") {
        if (is.na(spawnareas$X.1..Southwest.Coordinate[ii]) || is.na(spawnareas$X.1..Northeast.Coordinate))
            print(paste("Verdict Google maps on row", ii, "with NA coordinates"))
    } else if (substring(spawnareas$Verdict[ii], nchar(spawnareas$Verdict[ii])-3) == ".shp") {
        ## Pass
    }
}

write.csv(spawnareas, "inputs/Master Spawning ProCreator.csv", row.names=F)
