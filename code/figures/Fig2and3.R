library(ggplot2)
library(viridis)
library(sf)
library(scales)
library(dplyr)
library(RColorBrewer)

setwd("/Users/KO/Dropbox/Spawning ProCreator/dataset")
rm(list=ls())


# Figure 2: Number of species by functional group
# from sumstats.csv
stats_fn_grp<-read.csv("sumstats.csv")
stats_fn_grp$CONTINENT[is.na(stats_fn_grp$CONTINENT)] <- 'Multiple continents'
stats_fn_grp$CONTINENT[stats_fn_grp$CONTINENT== "Seven seas (open ocean)"] <- 'Other islands'
stats_fn_grp$CONTINENT =
  factor(stats_fn_grp$CONTINENT, levels =
           c('Africa', 'Asia', 'Europe', 'North America', 'South America',
             'Oceania', 'Antarctica', 'Other islands', 'Multiple continents'))

plot_with_na<-ggplot(stats_fn_grp, aes(fill=commercial.group, y=spawnings.included, x=CONTINENT))
plot_with_na +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c('#2166AC', '#E15759', '#67001F', '#4E79A7', '#FDDBC7', '#D1E5F0', '#D6604D', '#B2182B', '#053061', '#4393C3'), name="") +
  theme_classic() +
  theme(legend.position="bottom") +
  xlab("") +
  ylab("Number of spawning region entries") +
  scale_x_discrete(labels = wrap_format(20))

stats_fn_grp %>%
  group_by(CONTINENT) %>%
  summarise(Frequency = sum(spawnings.included, na.rm=TRUE))


