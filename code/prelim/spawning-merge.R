library(PBSmapping) # GIS mapping library

## Load the country definitions
regions <- importShapefile("inputs/shapefiles/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014")
polydata <- attr(regions, 'PolyData')

anadromous <- readLines("inputs/anadromous.txt")
source("code/prelim/tools_spawning.R")

df <- data.frame(species=c(), country=c(), localities=c(), source=c())

## Add on all FishBase data
for (filename in list.files("inputs/spawning")) {
    species <- gsub("-", " ", substr(filename, 1, nchar(filename) - 4))
    if (species %in% anadromous)
        next
    print(species)

    ## Read this spawning data
    tbl <- read.csv(paste0("inputs/spawning/", filename))
    if (nrow(tbl) == 0)
        next
    tbl$Locality <- as.character(tbl$Locality)

    ## Standardize results
    for (ii in 1:nrow(tbl)) {
        bymonth <- combinespawning(tbl[ii,])
        if (sum(bymonth) == 0)
            bymonth <- rep(NA, 12)
        df <- rbind(df, data.frame(t(bymonth), species, country=tbl$Country[ii], localities=tbl$Locality[ii], source="Fishbase"))
    }
}

## Add on SCRFA records

monthnames <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

tbl <- read.csv("inputs/scrfa.csv")
tbl$fullspecies <- paste(tbl$Genus, tbl$Species)

for (ii in 1:nrow(tbl)) {
    if (tbl$fullspecies[ii] %in% anadromous)
        next

    if (ii %% 100 == 0)
        print(ii)
    months <- strsplit(as.character(tbl$SpawningMonths[ii]), " ")[[1]]
    bymonth <- rep(0, 12)
    for (month in months)
        bymonth[which(monthnames == month)] <- 100

    if (sum(bymonth) == 0)
        bymonth <- rep(NA, 12)

    df <- rbind(df, data.frame(t(bymonth), species=tbl$fullspecies[ii], country=tbl$CountryName[ii], localities=NA, source="SCRFA"))
}

nrow(df)
length(unique(df$species))
length(unique(paste(df$country, df$localities, sep=': ')))

write.csv(df, "outputs/spawning-records.csv", row.names=F)
