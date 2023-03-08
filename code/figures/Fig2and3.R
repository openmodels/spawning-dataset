library(ggplot2)
library(viridis)
library(sf)
library(scales)

setwd("/Users/KO/Dropbox/Spawning ProCreator/dataset")
rm(list=ls()) 


# Figure 2: Number of species by functional group
# from sumstats.csv
stats_fn_grp<-read.csv("sumstats.csv")
stats_fn_grp$CONTINENT[is.na(stats_fn_grp$CONTINENT)] <- 'Multiple continents'
stats_fn_grp$CONTINENT[stats_fn_grp$CONTINENT== "Seven seas (open ocean)"] <- 'non-Oceania islands'
stats_fn_grp$CONTINENT = 
  factor(stats_fn_grp$CONTINENT, levels = 
           c('Africa', 'Asia', 'Europe', 'North America', 'South America', 
             'Oceania', 'Antarctica', 'non-Oceania islands', 'Multiple continents'))

plot_with_na<-ggplot(stats_fn_grp, aes(fill=functional.group, y=species.spawning, x=CONTINENT))
plot_with_na + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  theme(legend.position="bottom") +
  xlab("") + 
  ylab("Number of species spawning-regions") +
  scale_x_discrete(labels = wrap_format(15))



# Figure 3: Number of entries classified for type of geocoding method
# from GeoSpawn.shp
setwd("/Users/KO/Dropbox/Spawning ProCreator/dataset")
rm(list=ls()) 
procreator <- st_read("GeoSpawn.shp")

#relabel the data to match the text of the paper
procreator$method[procreator$method == 'Hand-geocoded'] <- 'Geocoding: Manual'
procreator$method[procreator$method == 'ArcGIS geocoding'] <- 'Geocoding: ArcGIS'
procreator$method[procreator$method == 'GeoNames geocoding'] <- 'Geocoding: GeoNames'
procreator$method[procreator$method == 'Exclusive economic zone'] <- 'EEZ'
procreator$method[procreator$method == 'Multiple EEZs by FAO region'] <- 'Regional waters'
procreator$method[procreator$method == 'Near-shore region'] <- 'EEZ near-shore polygon'
procreator$method[procreator$method == 'Bathymetry-based polygon'] <- 'EEZ-bathymetry polygon'

plot_data_class<-ggplot(procreator, aes(method))
plot_data_class+geom_bar()+theme_classic()+
  theme(legend.position="bottom")+
  xlab("Geocoding method")+
  ylab("Number of species spawning-regions")+
  scale_x_discrete(labels = wrap_format(15))

#Figure 3: Percentage of entries classified for type of geocoding method
#Using Spawning ProCreator google sheet 
setwd("/Users/KO/Dropbox/Spawning ProCreator/dataset")
rm(list=ls()) 
procreator<-read.csv("MasterSpawningProCreator.csv")

#clean the data
procreator$Verdict[procreator$Verdict == 'EEZs'] <- 'EEZ'
procreator$Verdict[procreator$Verdict == 'near-shore'] <- 'EEZ'
procreator$Verdict[procreator$Verdict == 'FAO sheet + Bathymetry'] <- 'Regional waters'
procreator$Verdict[procreator$Verdict == 'FAO sheet'] <- 'Regional waters'
procreator$Verdict[procreator$Verdict == 'native range'] <- 'Native range'
procreator$Verdict[procreator$Verdict == 'drop'] <- 'Dropped entries'
procreator$Verdict[procreator$Verdict == 'drop '] <- 'Dropped entries'
procreator$Verdict[procreator$Verdict == 'Drop'] <- 'Dropped entries'
procreator$Verdict[procreator$Verdict == 'red'] <- 'Coded regions'
procreator$Verdict[procreator$Verdict == 'green'] <- 'Coded regions'
procreator$Verdict[procreator$Verdict == 'Google maps'] <- 'Manual Geocoding'
procreator$Verdict[procreator$Verdict == 'chile-46.shp'] <- 'EEZ'
procreator$Verdict[procreator$Verdict == 'canada-477.shp'] <- 'EEZ'
procreator$Verdict[procreator$Verdict == 'norway-922.shp'] <- 'EEZ'
procreator$Verdict[procreator$Verdict == 'cuba-1101.shp'] <- 'EEZ'
procreator$Verdict[procreator$Verdict == 'newzealand-1728.shp'] <- 'EEZ'
procreator$Verdict[procreator$Verdict == 'jamaica-1839.shp'] <- 'EEZ'


plot_data_class<-ggplot(procreator, aes(x=Verdict,y=..prop.., group=1))
plot_data_class+geom_bar()+theme_classic()+theme(legend.position="bottom")+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Geocoding method")+ylab("Percentage of entries")

