setwd("~/Dropbox/Climate Change Fish Nets")
datapath <- "~/research/fishnets/"

source("code/spawning/read.R")
source("code/lib/hierarchy.R")

spawning <- read.csv("code/spawning-records.csv")
spawning <- spawning[!duplicated(spawning),]

groups <- read.csv(file.path(datapath, "saudata/groups.csv"))
groups$scientific <- as.character(groups$scientific)
groups$scientific[!is.na(groups$scientific_name)] <- as.character(groups$scientific_name[!is.na(groups$scientific_name)])
groups <- groups[!duplicated(groups),]

groups[groups$scientific %in% names(table(groups$scientific))[table(groups$scientific) > 1],]

spawning$commercial.group <- NA
spawning$functional.group <- NA
spawning$group.level <- NA
spawning$group.match <- NA
for (ii in 1:nrow(spawning)) {
    print(ii)
    founds <- lowest.hierarchy(spawning$species[ii], function(specwithin) {
        groups$commercial_group[groups$scientific %in% specwithin & !is.na(groups$commercial_group)]
    })
    if (length(founds$values) == 1) {
        spawning$commercial.group[ii] <- founds$values
        spawning$group.level[ii] <- founds$level
        spawning$group.match[ii] <- 1
    } else {
        chosen <- names(sort(table(founds$values), decreasing=T)[1])
        spawning$commercial.group[ii] <- chosen
        spawning$group.level[ii] <- founds$level
        spawning$group.match[ii] <- mean(chosen == founds$values)
    }

    founds <- lowest.hierarchy(spawning$species[ii], function(specwithin) {
        groups$functional_group[groups$scientific %in% specwithin & !is.na(groups$functional_group)]
    })
    if (length(founds$values) == 1) {
        spawning$functional.group[ii] <- founds$values
    } else {
        chosen <- names(sort(table(founds$values), decreasing=T)[1])
        spawning$functional.group[ii] <- chosen
    }
}

library(PBSmapping)
shp <- importShapefile(file.path(datapath, "shapefiles/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"))
polydata <- attr(shp, 'PolyData')

source("code/names.R")

spawning$canonical <- sapply(spawning$country, function(country) ifelse(is.na(country), NA, spawning2canonical(country)))
spawning$worldbank <- sapply(spawning$canonical, function(country) ifelse(is.na(country), NA, canonical2worldbank(country)))

spawning$NAME_CIAWF <- spawning$canonical
spawning$NAME_CIAWF[!(spawning$NAME_CIAWF %in% polydata$NAME_CIAWF)] <- spawning$worldbank[!(spawning$NAME_CIAWF %in% polydata$NAME_CIAWF)]
spawning$NAME_CIAWF[spawning$NAME_CIAWF == "Korea, Rep."] <- "Korea, South"
spawning$NAME_CIAWF[spawning$NAME_CIAWF == "Korea, Dem. Rep."] <- "Korea, North"

continents <- data.frame(NAME_CIAWF=polydata$NAME_CIAWF, CONTINENT=polydata$CONTINENT) %>% group_by(NAME_CIAWF) %>%
    summarize(CONTINENT=ifelse(length(unique(CONTINENT)) == 1, as.character(CONTINENT[1]), NA))
spawning2 <- spawning %>% left_join(continents, by='NAME_CIAWF')
spawning2$CONTINENT[spawning$country == "Azores Is."] <- "Seven seas (open ocean)"
spawning2$CONTINENT[spawning$country == "Micronesia"] <- "Oceania"
spawning2$CONTINENT[spawning$country == "Canary Is."] <- "Africa"
spawning2$CONTINENT[spawning$country == "S. Georg. Sandw."] <- "Seven seas (open ocean)"
spawning2$CONTINENT[spawning$country == "St Helena"] <- "Seven seas (open ocean)"
spawning2$CONTINENT[spawning$country == "Guinea-Bissau"] <- "Africa"
spawning2$CONTINENT[spawning$country == "Elephant I."] <- "Antarctica"
spawning2$CONTINENT[spawning$country == "Antarctica"] <- "Antarctica"
spawning2$CONTINENT[spawning$country == "Kerguelen Is."] <- "Seven seas (open ocean)"
spawning2$CONTINENT[spawning$country == "Heard McDon Is."] <- "Seven seas (open ocean)"
spawning2$CONTINENT[spawning$country == "South Shetland"] <- "Antarctica"
spawning2$CONTINENT[spawning$country == "Congo"] <- "Africa"
spawning2$CONTINENT[spawning$country == "Virgin Is. (US)"] <- "North America"
spawning2$CONTINENT[spawning$country == "Juan Fernández"] <- "South America"
spawning2$CONTINENT[spawning$country == "Cote d'Ivoire"] <- "Africa"
spawning2$CONTINENT[spawning$country == "South Orkney Is."] <- "Antarctica"
spawning2$CONTINENT[spawning$country == "Easter I."] <- "Oceania"
spawning2$CONTINENT[spawning$country == "Ryukyu Is."] <- "Asia"
spawning2$CONTINENT[spawning$country == "Faeroe Is."] <- "Europe"
spawning2$CONTINENT[spawning$country == "Falkland Is."] <- "South America"
spawning2$CONTINENT[spawning$country == "North Marianas"] <- "Oceania"
spawning2$CONTINENT[spawning$country == "Galapagos Is."] <- "South America"
spawning2$CONTINENT[spawning$country == "Terre Adélie"] <- "Antarctica"
spawning2$CONTINENT[spawning$country == "Crozet Is."] <- "Seven seas (open ocean)"
spawning2$CONTINENT[spawning$country == "Macedonia"] <- "Europe"
spawning2$CONTINENT[spawning$country == "Marquesas Is."] <- "Oceania"
spawning2$CONTINENT[spawning$country == "British Indian Ocean Territory"] <- "Seven seas (open ocean)"
spawning2$CONTINENT[spawning$country == "Micronesia, Federated States of"] <- "Oceania"
spawning2$CONTINENT[spawning$country == "Netherlands Antilles"] <- "South America"
spawning2$CONTINENT[spawning$country == "Virgin Islands (USA)"] <- "North America"

unique(spawning2$country[is.na(spawning2$CONTINENT)])

shp.ds <- importShapefile("~/Dropbox/Spawning ProCreator/dataset/GeoSpawn.shp")
polydata.ds <- attr(shp.ds, 'PolyData')

included <- paste(polydata.ds$species, polydata.ds$country, polydata.ds$localities)
spawning2$included <- paste(spawning2$species, spawning2$country, spawning2$localities) %in% included

sumstats <- spawning2 %>% group_by(commercial.group, functional.group, CONTINENT) %>%
    summarize(species.spawning=length(unique(species)), spawnings.spawning=length(species),
              species.included=length(unique(species[included])), spawnings.included=length(species[included]))

species <- read.csv(file.path(datapath, "saudata/species.csv"))
groups <- read.csv(file.path(datapath, "saudata/groups.csv"))
groups <- groups[!duplicated(groups),]

countries <- read.csv(file.path(datapath, "saudata/countries.txt"), sep="\t", header=F)
names(countries) <- c('eezpath', 'country')
species$eezpath <- paste0("~/eez/", species$eez, ".aspx")

species$commercial.group <- NA
species$functional.group <- NA
species$group.level <- NA
species$group.match <- NA
for (ii in 1:nrow(species)) {
    print(ii)
    founds <- lowest.hierarchy(species$scientific[ii], function(specwithin) {
        groups$commercial_group[groups$scientific %in% specwithin & !is.na(groups$commercial_group)]
    })
    if (length(founds$values) == 1) {
        species$commercial.group[ii] <- founds$values
        species$group.level[ii] <- founds$level
        species$group.match[ii] <- 1
    } else {
        chosen <- names(sort(table(founds$values), decreasing=T)[1])
        species$commercial.group[ii] <- chosen
        species$group.level[ii] <- founds$level
        species$group.match[ii] <- mean(chosen == founds$values)
    }

    founds <- lowest.hierarchy(species$scientific[ii], function(specwithin) {
        groups$functional_group[groups$scientific %in% specwithin & !is.na(groups$functional_group)]
    })
    if (length(founds$values) == 1) {
        species$functional.group[ii] <- founds$values
    } else {
        chosen <- names(sort(table(founds$values), decreasing=T)[1])
        species$functional.group[ii] <- chosen
    }
}

species2 <- species %>% left_join(countries)
species2$canonical <- sapply(species2$country, sau2canonical)
species2$worldbank <- sapply(species2$canonical, function(country) ifelse(is.na(country), NA, canonical2worldbank(country)))

species2$NAME_CIAWF <- species2$canonical
species2$NAME_CIAWF[!(species2$NAME_CIAWF %in% polydata$NAME_CIAWF)] <- species2$worldbank[!(species2$NAME_CIAWF %in% polydata$NAME_CIAWF)]

species3 <- species2 %>% left_join(continents, by='NAME_CIAWF')
species3$CONTINENT[species3$country %in% c('Micronesia (Federated States of)', 'Brunei Darussalam')] <- "Oceania"
species3$CONTINENT[species3$country %in% c("Myanmar", "Russia (Barents Sea)", "Russia (Black Sea)", "Russia (Far East)", "Russia (Kara Sea)", "Russia (Laptev to Chukchi Sea)", "Korea (North)", "Korea (South)", "Iran (Sea of Oman)", "Iran (Persian Gulf)", "Gaza Strip", "Yemen (Red Sea)", "Yemen (Arabian Sea)")] <- "Asia"
species3$CONTINENT[species3$country %in% c("Congo, R. of", "Congo (ex-Zaire)", "Côte d'Ivoire", "Egypt (Mediterranean)", "Egypt (Red Sea)")] <- "Africa"
species3$CONTINENT[species3$country %in% c("Saint Kitts &amp; Nevis", "Saint Vincent &amp; the Grenadines")] <- "North America"
species3$CONTINENT[species3$country %in% c("Russia (Baltic Sea)")] <- "Europe"

unique(species3$country[is.na(species3$CONTINENT)])

sumstats.sau <- species3 %>% group_by(commercial.group, functional.group, CONTINENT) %>%
    summarize(species.sau=length(unique(scientific)), stocks.sau=length(scientific))

sumstats2 <- sumstats %>% full_join(sumstats.sau)

write.csv(sumstats2, "~/Dropbox/Spawning ProCreator/dataset/sumstats.csv", row.names=F)
write.csv(species3, "~/Dropbox/Spawning ProCreator/dataset/sau-species.csv", row.names=F)
write.csv(spawning2, "~/Dropbox/Spawning ProCreator/dataset/spawning-species.csv", row.names=F)
