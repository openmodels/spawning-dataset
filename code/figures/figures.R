setwd("~/Dropbox/Climate Change Fish Nets")
datapath <- "~/research/fishnets/"

do.shapename <- "GO-FISH-hs"

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

## Try to assign continents to FAO sheet entries
specieseez <- read.csv("code/fao2eez/specieseez.csv")
eezshp <- importShapefile(file.path(datapath, "shapefiles/World_EEZ_v8_20140228_LR/World_EEZ_v8_2014.shp"), readDBF=T)
eezshp.polydata <- attr(eezshp, "PolyData")

missing.se <- c()
for (ii in which(is.na(spawning2$CONTINENT) | spawning2$CONTINENT == "")) {
    eezlist <- specieseez$eez[specieseez$region == spawning2$localities[ii] & specieseez$specie == spawning2$species[ii]]
    if (length(eezlist) == 0) {
        if (spawning2$localities[ii] %in% c("North Sea", "Caspian Sea (Volga)", "Europe", "Europe: Italy, Tunisia", "Bay of Biscay", "Lake Constance", "Northern Europe", "Lake Geneva", "European lakes.", "Faroe (NW, N and NE)", "northern North Sea and shelf West of Orkney and Shetland", "Bay of Biscay, 1996-1997", "Central Europe", "Lake Ohrid", "Danube drainage", "Lake Ohrid (Albania, Macedonia)", "Lake Prespa", "Barents Sea", "Southern North Sea", "Western Caspian Sea", "Adriatic basin", "From Po to Soca drainages", "Moraca and Neretva drainages (Bosnia-Herzegovina, Montenegro)"))
            spawning2$CONTINENT[ii] <- "Europe"
        else if (spawning2$localities[ii] %in% c("Caribbean sea", "Lake Superior", "Michigan and Huron Lakes", "off  Florida, Caribbean", "Bristol Bay northward to Nunivak Island", "Georges Bank, Browns Bank and coastal Gulf of Maine", "Grand Bank", "Scotian Banks", "Flathead River of British Columbia, Alberta and Montana.", "Yukon River", "Yukon", "Yukon drainage", "Middle Cove Beach", "Newfoundland"))
            spawning2$CONTINENT[ii] <- "North America"
        else if (spawning2$localities[ii] %in% c("Orinoco River", "Curaçao"))
            spawning2$CONTINENT[ii] <- "South America"
        else if (spawning2$localities[ii] %in% c("Seamounts off southern part of  Africa"))
            spawning2$CONTINENT[ii] <- "Africa"
        else if (spawning2$localities[ii] %in% c("Mekong mainstream", "Nizhnyaya Tunguska", "Western Pacific: around Commander Island and coast of Siberia", "Eastern Bering Sea (1959-1964)", "Eastern Bering Sea (1990)", "Anadyr River", "Korea", "Yenisei River, Siberia", "Syr-Darya", "Asia", "Ganges Hooghly system"))
            spawning2$CONTINENT[ii] <- "Asia"
        else if (spawning2$localities[ii] %in% c("Eastern Indian Ocean", "Southestern Indian Ocean", "Arctic Ocean", "Arctic Ocean basin", "Southern Emperor-Northern Hawaiian Ridge"))
            spawning2$CONTINENT[ii] <- "Seven seas (open ocean)"
        else {
            print(paste("None for", spawning2$localities[ii]))
            missing.se <- c(missing.se, spawning2$localities[ii])
        }
    } else {
        contlist <- unique((data.frame(NAME_CIAWF=eezshp.polydata$Country[eezlist]) %>% left_join(continents))$CONTINENT)
        contlist <- contlist[!is.na(contlist)]
        if (length(unique(contlist)) == 1)
            spawning2$CONTINENT[ii] <- unique(contlist)
    }
}
unique(missing.se)

unique(spawning2$country[is.na(spawning2$CONTINENT)])

polydata.ds <- read.csv(paste0("~/Dropbox/Spawning ProCreator/dataset/", do.shapename, ".csv"))

spawning2$included <- F
allmiss <- c()
for (ii in 1:nrow(polydata.ds)) {
    rows <- which(spawning2$species == polydata.ds$species[ii] &
                  (((is.na(polydata.ds$country[ii]) | polydata.ds$country[ii] == "") & (is.na(spawning2$country) | spawning2$country == "")) |
                   polydata.ds$country[ii] == spawning2$country) &
                  (((is.na(polydata.ds$localities[ii]) | polydata.ds$localities[ii] == "") & (is.na(spawning2$localities) | spawning2$localities == "")) |
                   polydata.ds$localities[ii] == spawning2$localities))
    if (length(rows) == 0)
        allmiss <- c(allmiss, ii)
    ## else if (length(rows) > 1) {
    ##     print("Duplicates!")
    ##     break
    ## }
    else
        spawning2$included[rows] <- T
}

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

length(unique(spawning2$species[spawning2$included]))
sum(spawning2$included)
