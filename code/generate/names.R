# Return list of SAU names
eez2sau <- function(eez) {
    if (eez == "Russia")
        return(c("Russia (Baltic Sea, Kaliningrad)", "Russia (Baltic Sea, St. Petersburg)", "Russia (Barents Sea)", "Russia (Black Sea)", "Russia (Pacific)", "Russia (Siberia)"))
    if (eez == "Cocos Islands")
        return("Cocos (Keeling) Isl. (Australia)")
    if (eez == "Christmas Island")
        return("Christmas Isl. (Australia)")
    if (eez == "Andaman and Nicobar")
        return("Andaman & Nicobar Isl. (India)")
    if (eez == "Indonesia")
        return(c("Indonesia (Eastern)", "Indonesia (Western)"))
    if (eez == "East Timor")
        return("Timor Leste")
    if (eez == "Malaysia")
        return(c("Malaysia (Peninsula East)", "Malaysia (Peninsula West)", "Malaysia (Sabah)", "Malaysia (Sarawak)"))
    if (eez == "Brunei")
        return("Brunei Darussalam")
    if (eez == "Clipperton Island")
        return("Clipperton Isl. (France)")
    if (eez == "United States")
        return(c("USA (East Coast)", "USA (Gulf of Mexico)", "USA (West Coast)"))
    if (eez == "Japan")
        return(c("Japan (main islands)", "Japan (outer islands)"))
    if (eez == "Comoro Islands")
        return("Comoros")
    if (eez == "Mayotte")
        return("Mayotte (France)")
    if (eez == "Northern Mariana Islands and Guam")
        return(c("Northern Marianas (USA)", "Guam (USA)"))
    if (eez == "North Korea")
        return("Korea (North)")
    if (eez == "South Korea")
        return("Korea (South)")
    if (eez == "Turkey")
        return(c("Turkey (Black Sea)", "Turkey (Mediterranean Sea)"))
    if (eez == "Svalbard")
        return("Svalbard Isl. (Norway)")
    if (eez == "Alaska")
        return("Alaska (USA)")
    if (eez == "Galapagos Islands")
        return("Galapagos Isl. (Ecuador)")
    if (eez == "Faeroe Islands")
        return("Faeroe Isl. (Denmark)")
    if (eez == "Jan Mayen")
        return("Jan Mayen Isl. (Norway)")
    if (eez == "Greenland")
        return("Greenland (Denmark)")
    if (eez == "Saint Pierre and Miquelon")
        return("Saint Pierre & Miquelon (France)")
    if (eez == "Norfolk Island")
        return("Norfolk Isl. (Australia)")
    if (eez == "New Caledonia")
        return("New Caledonia (France)")
    if (eez == "Solomon Islands")
        return("Solomon Isl.")
    if (eez == "Turks and Caicos Islands")
        return("Turks & Caicos Isl. (UK)")
    if (eez == "Cayman Islands")
        return("Cayman Isl. (UK)")
    if (eez == "Bermuda")
        return("Bermuda (UK)")
    if (eez == "Madeira")
        return("Madeira Isl. (Portugal)")
    if (eez == "Marshall Islands")
        return("Marshall Isl.")
    if (eez == "Howland Island and Baker Island")
        return("Howland & Baker Isl. (USA)")
    if (eez == "Puerto Rico")
        return("Puerto Rico (USA)")
    if (eez == "British Virgin Islands")
        return("British Virgin Isl. (UK)")
    if (eez == "Anguilla")
        return("Anguilla (UK)")
    if (eez == "Saint Kitts and Nevis")
        return("Saint Kitts & Nevis")
    if (eez == "Antigua and Barbuda")
        return("Antigua & Barbuda")
    if (eez == "Montserrat")
        return("Montserrat (UK)")
    if (eez == "Trinidad and Tobago")
        return("Trinidad & Tobago")
    if (eez == "Saint Vincent and the Grenadines")
        return("Saint Vincent & the Grenadines")
    if (eez == "Guadeloupe")
        return("Guadeloupe (France)")
    if (eez == "Saba")
        return("Saba (Windward Netherlands Antilles)")
    if (eez == "Aruba")
        return("Aruba (Leeward Netherlands Antilles)")
    if (eez == "Martinique")
        return("Martinique (France)")
    if (eez == "Virgin Islands of the United States")
        return("US Virgin Isl. (USA)")
    if (eez == "French Guiana")
        return("French Guiana (France)")
    if (eez == "Wallis and Futuna")
        return("Wallis & Futuna Isl. (France)")
    if (eez == "Saudi Arabia")
        return(c("Saudi Arabia (Persian Gulf)", "Saudi Arabia (Red Sea)"))
    if (eez == "R\xe9union")
        return("Réunion (France)")
    if (eez == "Ile Tromelin")
        return("Tromelin Isl. (France)")
    if (eez == "Johnston Atoll")
        return("Johnston Atoll (USA)")
    if (eez == "Hawaii")
        return(c("Hawaii Main Islands (USA)", "Hawaii Northwest Islands (USA)"))
    if (eez == "Western Sahara")
        return("Western Sahara (Morocco)")
    if (eez == "Guinea Bissau")
        return("Guinea-Bissau")
    if (eez == "Azores")
        return("Azores Isl. (Portugal)")

    as.character(eez)
}

spawning2canonical <- function(country) {
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

    return(country)
}

canonical2worldbank <- function(country) { # This is to sovereigns
    if (country == "Bahamas")
        return("Bahamas, The")
    if (country %in% c("Bosnia & Herzegovina", "Bosnia Herzegovina"))
        return("Bosnia and Herzegovina")
    if (country %in% c("Egypt", "Egypt (Mediterranean)", "Egypt (Red Sea)"))
        return("Egypt, Arab Rep.")
    if (country %in% c("Iran", "Iran (Persian Gulf)", "Iran (Sea of Oman)"))
        return("Iran, Islamic Rep.")
    if (country == "Saint Lucia")
        return("St. Lucia")
    if (country == "Venezuela")
        return("Venezuela, RB")
    if (country == "Viet Nam")
        return("Vietnam")
    if (country == "Fmr Sudan")
        return("Sudan")
    if (country == "United Rep. of Tanzania")
        return("Tanzania")
    if (country %in% c("Yemen", "Yemen (Arabian Sea)", "Yemen (Red Sea)"))
        return("Yemen, Rep.")
    if (country == "Cape Verde")
      return("Cabo Verde")
    if (country %in% c("Congo, R. of", "Congo (Rep.)"))
        return("Congo, Rep.")
    if (country %in% c("Congo (ex-Zaire)", "Congo (DemRep)", "Congo"))
        return("Congo, Dem. Rep.")
    if (country == "Gambia")
        return("Gambia, The")
    if (country %in% c("Hong Kong", "China, Hong Kong SAR", "Hong Kong (China)"))
      return("Hong Kong SAR, China")
    if (country == "Marshall Isl.")
      return("Marshall Islands")
    if (country %in% c("Micronesia", "Micronesia (Federated States of)", "FS Micronesia"))
        return("Micronesia, Fed. Sts.")
    if (country %in% c("Saint Kitts & Nevis", "Saint Kitts and Nevis"))
        return("St. Kitts and Nevis")
    if (country %in% c("Saint Vincent & the Grenadines", "St Vincent", "Saint Vincent and the Grenadines"))
        return("St. Vincent and the Grenadines")
    if (country %in% c("Sao Tome & Principe", "Sao Tome Prn"))
        return("Sao Tome and Principe")
    if (country %in% c("Solomon Isl.", "Solomon Isds"))
      return("Solomon Islands")
    if (country == "Syria")
        return("Syrian Arab Republic")
    if (country == "Dominican Rep.")
        return("Dominican Republic")
    if (country %in% c("Timor Leste", "East Timor"))
        return("Timor-Leste")
    if (country == "Trinidad & Tobago")
        return("Trinidad and Tobago")
    if (country == "Antigua & Barbuda")
        return("Antigua and Barbuda")
    if (country %in% c("Andaman &amp; Nicobar Isl. (India)", "Andaman & Nicobar Isl. (India)", "Andaman and Nicobar"))
      return("India")
    if (country %in% c("Australia (Christmas Isl.)", "Australia (Cocos (Keeling) Isl.)", "Australia (Heard &amp; McDonald Isl.)",
                       "Australia (Lord Howe Isl.)", "Australia (Macquarie Isl.)", "Christmas Isl. (Australia)",
                       "Australia (Cocos (Keeling) Isl.)", "Heard & McDonald Isl. (Australia)",
                       "Lord Howe Isl. (Australia)", "Macquarie Isl. (Australia)", "Norfolk Isl. (Australia)", "Cocos Islands", "Christmas Island",
                       "Cocos (Keeling) Isl. (Australia)"))
        return("Australia")
    if (country %in% c("Trindade & Martin Vaz Isl. (Brazil)", "Brazil (mainland)", "Fernando de Noronha (Brazil)", "St Paul and St. Peter Archipelago (Brazil)", "Trindade & Martim Vaz Isl. (Brazil)"))
        return("Brazil")
    if (country %in% c("Chile (Easter Isl.)", "Chile (J. Fernandez, Felix and Ambrosio Isl.)", "Desventuradas Isl. (Chile)",
                       "Easter Isl. (Chile)", "J. Fernandez, Felix and Ambrosio Isl. (Chile)",
                       "Chile (mainland)", "Juan Fernandez Islands (Chile)"))
        return("Chile")
    if (country == "Bolivia (Plurinational State of)")
        return("Bolivia")
    if (country %in% c("Taiwan"))
      return("China")
    if (country %in% c("Greenland", "Faeroe Isl. (Denmark)", "Denmark (Baltic Sea)", "Denmark (North Sea)", "Greenland (Denmark)"))
      return("Denmark")
    if (country %in% c("Ecuador (Galapagos Isl.)", "Galapagos Isl. (Ecuador)", "Ecuador (mainland)"))
      return("Ecuador")
    if (country %in% c("France (French Guiana)", "French Guiana (France)", "France (French Polynesia)", "French Polynesia (France)", "France (Guadeloupe)",
                       "Guadeloupe", "France (Martinique)", "Martinique",
                       "France (Mayotte)", "France (New Caledonia)", "France (Reunion)",
                       "Amsterdam & St Paul Isl. (France)", "New Caledonia",
                       "Clipperton Isl. (France)", "Crozet Isl. (France)", "French Guiana", "Guadeloupe (France)",
                       "Kerguelen Isl. (France)", "Martinique (France)", "Mayotte (France)", "Mozambique Channel Isl. (France)",
                       "New Caledonia (France)", "Reunion (France)", "Saint Pierre & Miquelon (France)", "Tromelin Isl. (France)",
                       "Wallis & Futuna Isl. (France)", "Wallis and Futuna", "Corsica (France)",
                       "France (Atlantic Coast)", "France (Mediterranean)", "St Barthelemy (France)",
                       "Clipperton Isl.  (France)", "Glorieuse Islands (France)", "Guadeloupe  (France)",
                       "Réunion (France)", "St Martin (France)", "St Paul & Amsterdam Isl. (France)"))
      return("France")
    if (country %in% c("Haiti (Navassa Isl.)", "Navassa Isl. (Haiti)"))
        return("Haiti")
    if (country %in% c("Indonesia (Eastern)", "Indonesia (Western)", "Indonesia (Central)", "Indonesia (Indian Ocean)"))
        return("Indonesia")
    if (country %in% c("Japan (main islands)", "Japan (outer islands)", "Japan (Daito Islands)", "Japan (Ogasawara Islands)"))
        return("Japan")
    if (country %in% c("Korea (North)", "North Korea", "Korea, Dem. Rep."))
        return("Korea, Dem. Rep.")
    if (country %in% c("Korea (South)", "South Korea", "Korea South", "Rep. of Korea", "Korea, Rep."))
      return("Korea, Rep.")
    if (country %in% c("Malaysia (Peninsula East)", "Malaysia (Peninsula West)", "Malaysia (Sabah)", "Malaysia (Sarawak)"))
        return("Malaysia")
    if (country %in% c("Western Sahara (Morocco)", "Western Sahara", "Morocco (Central)", "Morocco (Mediterranean)", "Morocco (South)"))
        return("Morocco")
    if (country %in% c("Leeward Netherlands Antilles", "Windward Netherlands Antilles", "Aruba (Netherlands)", "Bonaire (Netherlands)", "Curacao (Netherlands)", "Saba and Sint Eustaius (Netherlands)", "Sint Maarten (Netherlands)"))
        return("Netherlands")
    if (country %in% c("Cook Isl. (New Zealand)", "Kermadec Isl. (New Zealand)", "New Zealand (Niue)", "New Zealand (Cook Isl.)",
                       "New Zealand (Tokelau)", "Niue (New Zealand)", "Tokelau (New Zealand)", "Cook Islands"))
      return("New Zealand")
    if (country %in% c("Bouvet Isl. (Norway)", "Norway (Svalbard Isl.)", "Jan Mayen Isl. (Norway)", "Svalbard Isl. (Norway)"))
      return("Norway")
    if (country %in% c("Azores Isl. (Portugal)", "Madeira Isl. (Portugal)", "Madeira",
                       "Portugal (mainland)"))
      return("Portugal")
    if (country %in% c("Russia (Baltic Sea, St. Petersburg)", "Russia (Barents Sea)", "Russia (Pacific)",
                       "Russia (Baltic Sea, Kaliningrad)", "Russia (Black Sea)", "Russia (Siberia)", "Russian Fed", "Russia", "Russia (Baltic Sea)", "Russia (Far East)", "Russia (Kara Sea)", "Russia (Laptev to Chukchi Sea)"))
      return("Russian Federation")
    if (country %in% c("Saudi Arabia (Red Sea)", "Saudi Arabia (Persian Gulf)"))
        return("Saudi Arabia")
    if (country %in% c("Prince Edward Isl. (South Africa)", "South Africa (Atlantic and Cape)", "South Africa (Indian Ocean Coast)"))
        return("South Africa")
    if (country %in% c("Canary Isl. (Spain)", "Balearic Island (Spain)", "Spain (mainland, Med and Gulf of Cadiz)", "Spain (Northwest)"))
      return("Spain")
    if (country %in% c("Turkey (Mediterranean Sea)", "Turkey (Black Sea)", "Turkey (Marmara Sea)"))
      return("Turkey")
    if (country %in% c("Anguilla (UK)", "Anguilla", "Bermuda (UK)", "Bermuda",
                       "British Virgin Isl. (UK)",
                       "British Virgin Islands", "Cayman Isl. (UK)", "Cayman Islands",
                       "Chagos Archipel., Brit. Ind. Oc. Terr. (UK)", "Falkland Isl. (UK)",
                       "Montserrat (UK)", "Montserrat",
                       "Saint Helena (UK)", "Tristan da Cunha Isl. (UK)", "Ascension Isl. (UK)", "Bermuda (UK)",
                       "British Virgin Isl. (UK)", "Cayman Isl. (UK)", "Chagos Archipel., Brit. Ind. Oc. Terr. (UK)",
                       "Channel Isl. (UK)", "Falkland Isl. (UK)", "Pitcairn (UK)", "South Georgia & Sandwich Isl. (UK)",
                       "Turks & Caicos Isl. (UK)", "United Kingdom (UK)", "South Orkney Islands (UK)",
                       "Chagos Archipelago (UK)", "Virgin Islands (British)"))
      return("United Kingdom")
    if (country %in% c("Hawaii", "Alaska", "Alaska (USA)", "Guam (USA)", "Jarvis Isl. (USA)", "Johnston Atoll (USA)", "Northern Marianas (USA)",
                       "Puerto Rico", "Puerto Rico (USA)", "US Virgin Isl.", "US Virgin Isl. (USA)", "Guam (USA)", "Hawaii Main Islands (USA)",
                       "Hawaii Northwest Islands (USA)", "Howland & Baker Isl. (USA)", "Jarvis Isl. (USA)",
                       "Johnston Atoll (USA)", "Palmyra Atoll & Kingman Reef (USA)", "United States, East Coast",
                       "United States, Gulf of Mexico", "United States, West Coast", "Wake Isl. (USA)", "USA",
                       "USA (East Coast)", "USA (Gulf of Mexico)", "USA (West Coast)", "USA (Alaska, Arctic)",
                       "USA (Alaska, Subarctic)", "American Samoa (USA)"))
        return("United States")
    ## if (country %in% c("Gaza Strip"))
    ##     return("West Bank and Gaza")
    if (country %in% c("Canada (Arctic)", "Canada (East Coast)", "Canada (Pacific)"))
        return("Canada")
    if (country %in% c("Mexico (Atlantic)", "Mexico (Pacific)"))
        return("Mexico")
    if (country %in% c("Oman (Musandam)"))
        return("Oman")
    if (country %in% c("United Arab Emirates (Fujairah)"))
        return("United Arab Emirates")
    if (country %in% c("Thailand (Andaman Sea)", "Thailand (Gulf of Thailand)"))
        return("Thailand")
    if (country %in% c("Honduras (Caribbean)", "Honduras (Pacific)"))
        return("Honduras")
    if (country %in% c("Guatemala (Caribbean)", "Guatemala (Pacific)"))
        return("Guatemala")
    if (country %in% c("Colombia (Caribbean)", "Colombia (Pacific)"))
        return("Colombia")
    if (country %in% c("Nicaragua (Caribbean)", "Nicaragua (Pacific)"))
        return("Nicaragua")
    if (country %in% c("Panama (Caribbean)", "Panama (Pacific)"))
        return("Panama")
    if (country %in% c("Costa Rica (Caribbean)", "Costa Rica (Pacific)"))
        return("Costa Rica")
    if (country %in% c("Sweden (Baltic)", "Sweden (West Coast)"))
        return("Sweden")
    if (country %in% c("Italy (mainland)", "Sardinia (Italy)", "Sicily (Italy)"))
        return("Italy")
    if (country %in% c("Germany (Baltic Sea)", "Germany (North Sea)"))
        return("Germany")
    if (country %in% c("Greece (without Crete)", "Crete (Greece)"))
        return("Greece")
    if (country %in% c("Cyprus (North)", "Cyprus (South)"))
        return("Cyprus")
    if (country %in% c("West Bank and Gaza", "Israel (Mediterranean)", "Israel (Red Sea)"))
        return("Israel")
    if (country %in% c("Kiribati (Gilbert Islands)", "Kiribati (Line Islands)", "Kiribati (Phoenix Islands)"))
        return("Kiribati")

    if (length(grep("\\(", country)) == 1)
        stop(paste0("Cannot find sovereign for ", country))

    return(as.character(country))
}

sau2canonical <- function(country) {
    if (country %in% c("China (Hong Kong)", "Hong Kong (China)"))
        return("Hong Kong")
    if (country == "Comoros Isl.")
      return("Comoros")
    if (country == "CÃ´te d'Ivoire")
      return("Cote d'Ivoire")
    if (country %in% c("Denmark (Greenland)", "Greenland (Denmark)"))
      return("Greenland")
    if (country == "India (mainland)")
      return("India")
    if (country == "Viet Nam")
        return("Vietnam")

    return(gsub("&amp;", "&", as.character(country)))
}
