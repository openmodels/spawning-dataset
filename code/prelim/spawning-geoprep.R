library(maps)
library(dplyr)

dropbox.path <- "~/Dropbox/Spawning ProCreator/maps"
dropbox.url <- "https://www.dropbox.com/home/Spawning%20ProCreator/maps"

df <- read.csv("inputs/spawning-records.csv")
df$months <- rowSums(df[, 1:12] > 0, na.rm=T)
df$score <- ((rowSums(!is.na(df[, 1:12])) > 0) + 1) / 2
df$region <- paste(df$country, df$localities, sep=' :: ')
df$genus <- sapply(df$species, function(x) strsplit(as.character(x), " ")[[1]][1])

## For calculating the weights
catch <- read.csv("inputs/saudata/catches.csv", header=F)
df.catch <- data.frame(key=catch$V1, catch=rowSums(catch[, -1], na.rm=T))
df.catch2 <- df.catch %>% group_by(key) %>% summarize(catch=sum(catch))
value <- read.csv("inputs/saudata/values.csv", header=F)
df.value <- data.frame(key=value$V1, value=rowSums(value[, -1], na.rm=T))
df.value2 <- df.value %>% group_by(key) %>% summarize(value=sum(value))

df.saueez <- read.csv("inputs/saudata/species.csv")
df.sau <- df.saueez[!duplicated(df.saueez$key), -1] %>% left_join(df.catch2) %>% left_join(df.value2)

geos1 <- read.csv("inputs/localities-geonames.csv")
geos2 <- read.csv("inputs/localities-arcgis.csv")

regionslist <- unique(df$region)

results <- data.frame()
allrects <- data.frame()
for (rr in 1:length(regionslist)) {
    region <- regionslist[rr]

    ## Calculate the weight for this region
    part.catch <- 0
    part.value <- 0
    speclist.catch <- list()
    for (species in unique(df$species[df$region == region])) {
        genus.scale <- 1
        row <- df.sau[df.sau$scientific == as.character(species),]
        if (nrow(row) == 0) {
            genus <- strsplit(as.character(species), " ")[[1]][1]
            row <- df.sau[df.sau$scientific == genus,]
            genus.scale <- 1 / length(unique(df$species[df$genus == genus]))
            if (nrow(row) == 0)
                next
        }
        portion <- genus.scale * sum(df$score[df$species == species & df$region == region]) / sum(df$score[df$species == species])
        part.catch <- part.catch + row$catch * portion
        part.value <- part.value + row$value * portion
        speclist.catch[[species]] <- row$catch * portion
    }

    parts <- strsplit(region, " :: ")[[1]]
    country <- parts[1]
    localities <- parts[2]

    specstr <- paste(sapply(names(speclist.catch), function(species) paste0(species, " (", round(100 * speclist.catch[[species]] / part.catch), "%)")), collapse=", ")

    master.rect1 <- rep(NA, 4) # WSEN
    master.rect2 <- rep(NA, 4) # WSEN

    ## Create the map with geocodings

    pdf(file.path(dropbox.path, paste0("map", rr, ".pdf")), width=6, height=3)
    par(rep(0, 4))
    map("world", mar=rep(0, 4))

    ## Plot goes 1
    g1row <- geos1[((!is.na(geos1$country) & geos1$country == country) | (is.na(geos1$country) & country == "NA")) &
                   ((!is.na(geos1$localities) & geos1$localities == localities) | (is.na(geos1$localities) & localities == "NA")),]
    if (nrow(g1row) > 0) {
        if (is.na(g1row$ne_lon) || g1row$ne_lon - g1row$sw_lon < .25 || g1row$ne_lat - g1row$sw_lat < .25) {
            if (!is.na(g1row$lon)) {
                points(g1row$lon, g1row$lat, col=2)
                master.rect1 <- c(g1row$lon - .25, g1row$lat - .25, g1row$lon + .25, g1row$lat + .25)
            }
            if (!is.na(g1row$ne_lon)) {
                points((g1row$ne_lon + g1row$sw_lon) / 2, (g1row$ne_lat + g1row$sw_lat) / 2, col=2)
                master.rect1 <- c(g1row$sw_lon - .125, g1row$sw_lat - .125, g1row$ne_lon + .125, g1row$ne_lat + .125)
            }
        } else if (g1row$ne_lon < g1row$sw_lon) {
            rect(g1row$ne_lon, g1row$ne_lat, -300, g1row$sw_lat, border=2)
            rect(300, g1row$ne_lat, g1row$sw_lon, g1row$sw_lat, border=2)
            master.rect1 <- c(-180, g1row$sw_lat, 180, g1row$ne_lat)
        } else {
            rect(g1row$ne_lon, g1row$ne_lat, g1row$sw_lon, g1row$sw_lat, border=2)
            master.rect1 <- c(g1row$sw_lon, g1row$sw_lat, g1row$ne_lon, g1row$ne_lat)
        }
    }

    ## Plot goes 2
    g2row <- geos2[((!is.na(geos2$country) & geos2$country == country) | (is.na(geos2$country) & country == "NA")) &
                   ((!is.na(geos2$localities) & geos2$localities == localities) | (is.na(geos2$localities) & localities == "NA")),]
    if (nrow(g2row) > 0 && !is.na(g2row$confidence)) {
        if (is.na(g2row$ne_lon) || g2row$ne_lon - g2row$sw_lon < .25 || g2row$ne_lat - g2row$sw_lat < .25) {
            if (!is.na(g2row$lon)) {
                points(g2row$lon, g2row$lat, col=3)
                master.rect2 <- c(g2row$sw_lon - .125, g2row$sw_lat - .125, g2row$ne_lon + .125, g2row$ne_lat + .125)
            }
        } else if (g2row$ne_lon < g2row$sw_lon) {
            rect(g2row$ne_lon, g2row$ne_lat, -300, g2row$sw_lat, border=3)
            rect(300, g2row$ne_lat, g2row$sw_lon, g2row$sw_lat, border=3)
            master.rect2 <- c(-180, g2row$sw_lat, 180, g2row$ne_lat)
        } else {
            rect(g2row$ne_lon, g2row$ne_lat, g2row$sw_lon, g2row$sw_lat, border=3)
            master.rect2 <- c(g2row$sw_lon, g2row$sw_lat, g2row$ne_lon, g2row$ne_lat)
        }
    }

    dev.off()

    ## Create the Map Tool link

    master.rect <- c(min(master.rect1[1], master.rect2[1]), min(master.rect1[2], master.rect2[2]),
                     max(master.rect1[3], master.rect2[3]), max(master.rect1[4], master.rect2[4]))
    if (is.na(master.rect[1]))
        url <- ""
    else
        url <- paste0("https://iridl.ldeo.columbia.edu/maproom/Global/Ocean_Temp/Annual_Cycle_Max.html?bbox=bb%3A", master.rect[1], "%3A", master.rect[2], "%3A", master.rect[3], "%3A", master.rect[4], "%3Abb")

    ## Create the final row in the spreadsheet

    results <- rbind(results, data.frame(country, localities, part.catch, part.value, species=specstr, geocode=paste0(dropbox.url, "?preview=map", rr, ".pdf"), geotool=url))
    allrects <- rbind(allrects, data.frame(country, localities, which='red', west=master.rect1[1], south=master.rect1[2], east=master.rect1[3], north=master.rect1[4]),
                      data.frame(country, localities, which='green', west=master.rect2[1], south=master.rect2[2], east=master.rect2[3], north=master.rect2[4]))
}

## Order by part.catch
results <- results[order(results$part.catch, decreasing=T),]
results$verdict <- ""
results$notes <- ""

write.csv(results, "outputs/master.csv", row.names=F)
write.csv(allrects, "outputs/allrects.csv", row.names=F)
