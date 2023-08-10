library(dplyr)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)

get.suitability <- function(filepath) {
    tbl <- get.table(filepath)

    lons <- seq(-179.75, 180, by=.5)
    lats <- seq(-89.75, 90, by=.5)
    img <- matrix(0, length(lons), length(lats))

    for (ii in 1:nrow(tbl))
        img[which(lons == tbl$Center.Long[ii]), which(lats == tbl$Center.Lat[ii])] <- tbl$Overall.Probability[ii]

    return(img)
}

get.suitability.referenced <- function(filepath, gridref, is.future=F) {
    if (is.future) {
        tbl <- read.csv(filepath)
        tbl$Overall.Probability <- tbl$Overall.Probability * tbl$future
    } else {
        tbl <- get.table(filepath)
    }

    suittbl <- gridref %>% left_join(tbl[, c('Center.Long', 'Center.Lat', 'Overall.Probability')], by=c('V1'='Center.Long', 'V2'='Center.Lat'))
    suittbl$Overall.Probability[is.na(suittbl$Overall.Probability)] <- 0

    return(suittbl$Overall.Probability)
}

get.table <- function(filepath) {
  ## Find the start of the table
  lines <- readLines(filepath)

  skip <- 0
  for (line in lines) {
      if (line %in% c("Genus,Species,Center Lat,Center Long,C-Square Code,Overall Probability,Depth,SST,Salinity,Primary Production,Sea Ice Concentration.,Distance to Land", "Genus,Species,Center Lat,Center Long,C-Square Code,Overall Probability,Depth,Temperature,Salinity,Primary Production,Sea Ice Concentration.,Distance to Land", "Genus,Species,Center Lat,Center Long,C-Square Code,Overall Probability"))
          break
      skip <- skip + 1
  }

  ## Look for the end of the data
  nrows <- 0
  for (line in lines[(skip + 2):length(lines)]) {
      if (line == "")
          break
      nrows <- nrows + 1
  }

  tbl <- read.csv(filepath, skip=skip, nrows=nrows)
  tbl$Center.Lat <- as.numeric(as.character(tbl$Center.Lat))
  tbl$Center.Long <- as.numeric(as.character(tbl$Center.Long))
  tbl$Overall.Probability <- as.numeric(as.character(tbl$Overall.Probability))
  return(tbl)
}

get.occurances <- function(filepath) {
    ## Find the start of the table
    lines <- readLines(filepath)

    skip <- 0
    for (line in lines) {
        if (stringr::str_sub(line, 1, nchar("Occurrence cells used")) == "Occurrence cells used")
            break
        skip <- skip + 1
    }

    tbl <- read.csv(filepath, skip=skip + 1, col.names=paste('X', 1:20)) # impose col.names to avoid special chars
    tbl$Center.Lat <- as.numeric(as.character(tbl$X.3))
    tbl$Center.Long <- as.numeric(as.character(tbl$X.4))
    tbl <- subset(tbl, !is.na(Center.Lat) & !is.na(Center.Long))
    return(data.frame(Center.Lat=tbl$Center.Lat, Center.Long=tbl$Center.Long, Overall.Probability=1))
}

make.probability <- function(values, condrow) {
    tprobs <- rep(NA, length(values))

    tprobs[values >= condrow$Pref.Min..10th. & values <= condrow$Pref.Max..90th.] <- 1
    tprobs[values < condrow$Min | values > condrow$Max] <- 0

    loslope <- values > condrow$Min & values < condrow$Pref.Min..10th.
    tprobs[loslope] <- (values[loslope] - condrow$Min) / (condrow$Pref.Min..10th. - condrow$Min)
    hislope <- values < condrow$Max & values > condrow$Pref.Max..90th.
    tprobs[hislope] <- (condrow$Max - values[hislope]) / (condrow$Max - condrow$Pref.Max..90th.)

    tprobs
}

get.conditions <- function(filepath) {
    ## Find the start of the table
    lines <- readLines(filepath)

    layer <- NA

    skip <- 0
    for (line in lines) {
        if (substr(line, 1, 10) == "Layer used")
            layer <- strsplit(line, ": ")[[1]][2]

        if (line == " ,Used,Min,Pref Min (10th),Pref Max (90th),Max")
            break
        skip <- skip + 1
    }

    ## Look for the end of the data
    nrows <- 0
    for (line in lines[(skip + 2):length(lines)]) {
        if (line == "")
            break
        nrows <- nrows + 1
  }

    tbl <- read.csv(filepath, skip=skip, nrows=nrows-1)
    tbl$layer <- layer
    return(tbl)
}

species.with.suitability <- function(suitdir) {
    filenames <- list.files(suitdir)
    gsub("\\.csv", "", gsub("-", " ", filenames))
}

##NR: Function to make a plottable version of probabilities
get.plottable <- function(probs,envindices,environment) {
  latcoords <- environment$CenterLat
  loncoords <- environment$CenterLong
  uniquelats <- sort(unique(latcoords))
  uniquelons <- sort(unique(loncoords))
  # Test plot
  plotprobs1 <- rep(0L,length(uniquelats)*length(uniquelons))
  plotprobs1[envindices] <- probs
  plotprobs <- matrix(0L, nrow=length(unique(environment$CenterLong)), ncol=length(unique(environment$CenterLat)))
  #counter <- 1
  for (ii in 1:length(uniquelons)) {
    thislon <- uniquelons[ii]
    for (jj in 1:length(uniquelats)) {
      thislat <- uniquelats[jj]
      index <- which(environment$CenterLat==thislat & environment$CenterLong==thislon)
      plotprobs[ii,jj] <- plotprobs1[index]
    }
  }
  return(plotprobs)
}
