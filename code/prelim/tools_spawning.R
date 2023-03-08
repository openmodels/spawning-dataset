combinespawning <- function(tbl) {
    bymonth <- rep(NA, 12)
    for (month in 1:12) {
        ## In each month, take the max of specifically given percentages
        if (sum(!is.na(tbl[, month]) & tbl[, month] != 111) > 0)
            bymonth[month] <- max(tbl[tbl[, month] != 111, month], na.rm=T)
        else
            bymonth[month] <- ifelse(sum(!is.na(tbl[, month])) > 0, 100, 0)
    }

    bymonth
}

country2indexes <- function(country, polydata) {
    #if (is.na(country) || country %in% c("Elephant I."))
     if (is.na(country))
      return(c())

    if (country == "USA")
        country <- "United States"
    if (country %in% c("UK", "UK Scotland", "UK Engld Wal", "UK No Ireld"))
        country <- "United Kingdom"
    if (country %in% c("Russian Fed", "Russian Federation"))
        country <- "Russia"
    if (country == "Guinea-Bissau")
        country <- "Guinea Bissau"
    if (country == "Korea (South)")
        country <- "South Korea"
    if (country == "Korea (North)")
        country <- "North Korea"
    if (country == "Azores Is.")
        country <- "Azores"
    if (country == "United Arab Em.")
        country <- "United Arab Emirates"
    if (country == "Solomon Is.")
        country <- "Solomon Islands"
    if (country == "Canary Is.")
        country <- "Canary Islands"
    if (country == "Niger")
        country <- "Nigeria"
    if (country == "Marshall Is.")
        country <- "Marshall Islands"
    if (country == "St Helena")
        country <- "Saint Helena"
    if (country == "Kerguelen Is.")
        country <- "Kerguelen Islands"
    if (country == "Heard McDon Is.")
        country <- "Heard and McDonald Islands"
    if (country == "Congo")
        country <- "R\xe9publique du Congo" # has more coast
    if (country == "Curaçao")
        country <- "Cura\xe7ao"
    if (country == "Cayman Is.")
        country <- "Cayman Islands"
    if (country %in% c("Virgin Is. (US)", "Virgin Islands (USA)"))
        country <- "Virgin Islands of the United States"
    if (country == "St Lucia")
        country <- "Saint Lucia"
    if (country == "Hong Kong")
        country <- "China"
    if (country == "Viet Nam")
        country <- "Vietnam"
    if (country == "Trinidad Tobago")
        country <- "Trinidad and Tobago"
    if (country == "Faeroe Is.")
        country <- "Faeroe Islands"
    if (country == "Virgin Is. (UK)")
        country <- "British Virgin Islands"
    if (country == "North Marianas")
        country <- "Northern Mariana Islands and Guam"
    if (country == "Falkland Is.")
        country <- "Falkland Islands"
    if (country == "Cote d'Ivoire")
        country <- "Ivory Coast"
    if (country == "S. Georg. Sandw.")
        country <- "South Georgia and the South Sandwich Islands"
    if (country == "Sao Tome Princ.")
        country <- "Sao Tome and Principe"
    if (country == "Easter I.")
        country <- "Easter Island"
    if (country == "Crozet Is.")
        country <- "Crozet Islands"
    if (country == "Kuril Is.")
      country <- "Southern Kuriles"
    if (country == "Madeira Is.")
      country <- "Madeira"
    if (country == "Galapagos Is.")
        country <- "Galapagos Islands"
    if (country == "Micronesia, Federated States of")
        country <- "Micronesia"
    #Not so sure about the following
    #if (country == "Elephant I." || country == "South Shetland" || country == "South Orkney Is." || country == "Terre Adélie")
    #  country <- "Antarctica"
    #if (country == "Marquesas Is.")
    #  country <- "French Polynesia"
    #if (country == "Juan Fernández")
    #  country <- "Chile"

    which(polydata$Country == as.character(country))
}

## country2indexes("Australia")

spawning2border <- function(spawning) {
    if (is.na(spawning) || spawning == "" || spawning == "Marquesas Is." || spawning == "Hong Kong")
        return(NA)
    if (spawning == "USA")
        return("United States")
    if (spawning == "UK" || spawning == "UK Scotland")
        return("United Kingdom")
    if (spawning == "Russian Fed")
        return("Russia")
    if (spawning == "Korea (North)")
        return("North Korea")
    if (spawning == "Korea (South)")
        return("South Korea")
    if (spawning == "Guinea-Bissau")
        return("Guinea Bissau")

    return(spawning)
}
