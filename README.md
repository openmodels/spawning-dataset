# spawning-dataset
Code for the generation and processing of fish spawning regions

## Organization
 - `code/` includes R scripts for both preparation and display.
 - `inputs/` contains data drawn from AquaMaps/FishBase or constructed
   by hand.
 - `outputs/` contains the shapefile dataset and other results.

## Reproduction process

First, clone the repository, and set the working directory in R to the
root of the repository. All R code assumes that this is the working
directory.
   
### To reproduce the Spawning ProCreator spreadsheet

The Spawning ProCreator spreadsheet, `Master Spawning ProCreator.csv`,
describes how each spawning region should be constructed. Improvements
to these constructions can be made by editing the spreadsheet, which
does not require reproducing it. However, if the underlying spawning
information from FishBase and SCRFA is extended, the spreadsheet
should be recreated and new rows should be merged with the existing
file.

To reproduce the spreadsheet, as it was prior to adding the
information that describes how regions should be constructed, follow
these steps:

1. You may optionally regenerate the mapping of EEZs to FAO
   regions. This is produced by `prelim/fao2eez/mapping.R`. After
   running it, move the resulting `outputs/fao2eez.csv` to
   `inputs/fao2eez.csv`.
   
2. Regenerate the `input/specieseez.csv` file if the `inputs/Region
FAO EEZ matching-DO NOT EDIT IN EXCEL.csv` has changed. This second
file describes the FAO regions corresponding to multinational
descriptions in the spawning dataset. To regenerate it, run
`prelim/fao2eez/species2eez.R`, which produces `output/specieseez.csv`
and move this file to `inputs/specieseez.csv`.

3. Merge the FishBase and SCRFA spawning records: Run the
   `code/prelim/spawning-merge.R` script. This generates a file
   `outputs/spawning-records.csv` which should be moved to
   `inputs/spawning-records.csv` for the next step.

4. Geocode spawning region names: Set `source = 'arcgis'` in
   `code/prelim/geocode.py` and run the script; then set `source =
   'geonames'` and run the script again. This script produces geocoded
   result files names `localities-arcgis.csv` and
   `localities-geonames.csv`. Move these to the `inputs/` directory.

5. Run the `code/prelim/spawning-geoprep.R` script, which constructs
   the raw Spawning ProCreator spreadsheet into
   `outputs/master.csv`. This can then be imported into Excel or
   Google Sheets for filling out the Verdict column.

6. When the Spawning ProCreator spreadsheet is prepared (the Verdict
   and other columns are manually entered), save the result as a CSV
   file at `inputs/Master Spawning ProCreator.csv`.

### Spawning ProCreator metadata

ID: Unique ID number assigned to each unique [Locality, Country] pair before manual verdict assignment (see verdict variable description below). During verdict assignment, if the researcher believed the location needed to be split into two entries, then the ID number was duplicated. 
Country: Spawning country listed in Fishbase.org’s spawning dataset
Localities: Locality listed in Fishbase.org’s spawning dataset.
Total Catch: Indicative catch associated with this spawning region, calculated by dividing the 2014 catch for each species associated with the spawning locality by the total number of spawning localities for that species, and summing these values across all of the species associated with the given locality. Catch values from Sea Around Us.
Total Value: Indicative value associated with this spawning region, calculated as in the Total Catch, but using landed values from Sea Around Us.
species: All species listed by scientific name that had the same [Locality, Country] from Fishbase.org’s spawning dataset. Percentages in parentheses describe the % of catch that each species contributed to the Total Catch value.
Geocoded: Maps produced by GeoNames Search Webservice and ArcGIS World Geocoding Service (documentation for these services can be found at https://www.geonames.org/export/web-services.html and https://developers.arcgis.com/documentation/mapping-apis-and-services/geocoding/, respectively). GeoNames were in red and ArcGIS were in green
Example of map: https://www.dropbox.com/home/Spawning%20ProCreator/maps?preview=map346.pdf
Verdict: Method researcher used to geocode the spawning location. See article’s method section for more details.
New Country: When there was no country in the description and the boundaries of a country’s EEZ would help with the accuracy of the geocoding, a country was used. An example is “Seamounts off southern part of Africa.” A seamount product was used, but South Africa’s EEZ helped to bound the seamounts. It was also used to override incorrect countries listed. When checked by a second researcher, if the original verdict was incorrect and should have been EEZ, this new decision overrode the original verdict by filling in “EEZ” here. Finally, if the description matched the native range better than any EEZ, “any” was written to indicate use the native range. 
(1) Southwest Coordinate, (1) Northeast Coordinate, (2) Southwest Coordinate, (2) Northeast Coordinate, (3) Southwest Coordinate, (3) Northeast Coordinate: There were up to 3 boxes used for these manual geocoding entries described in the methods of the paper. When the researcher manually created a box using Google Maps to bound the spawning grounds, the research recorded the two diagonal coordinates to draw the box. Coordinates with the same (#) are pairs. 
Latitude range: When the description included latitudes or areas that commonly use latitudes, we noted the latitude range here. We used 35oS - 35oN for the subtropics and tropics, 23.5oS - 23.5oN for the tropics, 20oS - 20oN for the equatorial region, and 30oS - 30oN for lower latitudes.
Notes: The researcher made notes of difficult entries, primary sources when they changed the description, or reasons why an entry was dropped. These notes are not comprehensive of the full discussion of all the challenging entries. 
Notes (4/13/23): During a last check of the data on April 13, 2023, there were some rows that were dropped in the data merging often due to punctuation differences. These notes corrected those issues. These notes were also used to replace any verdicts with a more accurate shapefile of a region using marineregions.org shapefiles. The most common shapefiles were seas, gulfs, and bays. 

### To reproduce the GO-FISH shapefile

The `code/generate/read.R` functions translate information from the
`inputs/Master Spawning ProCreator.csv` spreadsheet into shapefile
regions.

The `code/generate/publicdataset.R` script produces a shapefile that
includes all available spawning regions, intersected with suitability
information. It produces `outputs/GO-FISH.shp` and
`outputs/GO-FISH.csv`, the latter of which corresponds to the polygon
attributes in the shapefile.

The `code/generate/stats.R` script generates `spawning-species.csv`
which provides information about each species and spawning region
provided in the spawning regions dataset; `sau-species.csv` which
provides information about each species in the SAU dataset; and
`sumstats.csv` which provides a summary of this information by
continent and fish group.

To regenerate the public dataset, first run
`code/generate/publicdataset.R` and then `code/generate/stats.R`.

### To regenerate the figures

 - `maps.R` generates spawning maps across the whole year, by season,
   or by month.
   
 - `Fig2and3.R` generates the other figures in the paper.

## License

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

This repository contains data extracted from public datasets, as described below:

 - FishBase: The data under `inputs/spawning` and summarized in `inputs/spawning-records.csv` is derived from FishBase (CC-BY-NC 3.0).

   Froese, R. and D. Pauly. Editors. 2023. FishBase. World Wide Web electronic publication. www.fishbase.org, version (02/2023).

 - Sea Around Us: The data under `inputs/saudata` is derived from Sea Around Us (CC-BY-NC 4.0)

   Pauly D., Zeller D., Palomares M.L.D. (Editors), 2020. Sea Around Us Concepts, Design and Data (seaaroundus.org).
   
 - AquaMaps: The data under `inputs/ranges` is derived from AquaMaps (CC-BY-NC 3.0)

   Kaschner, K., Kesner-Reyes, K., Garilao, C., Segschneider, J., Rius-Barile, J. Rees, T., & Froese, R. (2019, October). AquaMaps: Predicted range maps for aquatic species. Retrieved from https://www.aquamaps.org.
   
 - Natural Earth: The shapefiles under `inputs/shapefiles/ne_10m_admin_0_countries` and `inputs/shapefiles/ne_50m_coastline` where made by Natural Earth (public domain): https://www.naturalearthdata.com/downloads/
 
 - SCRFA: The file `inputs/scrfa.csv` and the summarized dataset `inputs/spawning-records.csv` contains information derived from the SCRFA Aggregations Database: https://www.scrfa.org/database/

 - Marine Regions: Some shapefiles in `inputs/shapefiles` are extracted from Marine Regions (CC-BY-4.0): https://www.marineregions.org/downloads.php

 - Knolls and seamounts in the world ocean: Some shapefiles in `inputs/shapefiles` are extracted from Yesson et al. (2011) (CC-BY-3.0)
   
   Yesson, Chris; Clark, M R; Taylor, M; Rogers, A D (2011): Knolls and seamounts in the world ocean - links to shape, kml and data files. PANGAEA, https://doi.org/10.1594/PANGAEA.757563,

- Bathemetry:  Some shapefiles in `inputs/shapefiles` 2-minute Gridded Global Relief Data (ETOPO2) v2: https://doi.org/10.7289/V5J1012Q 

  NOAA National Geophysical Data Center. 2006: 2-minute Gridded Global Relief Data (ETOPO2) v2. NOAA National Centers for Environmental Information. https://doi.org/10.7289/V5J1012Q.
  
- FAO Major Fishing Areas: FAO regions are used to interpret international spawning regions.

   FAO 2023. FAO Major Fishing Areas. Fisheries and Aquaculture Division [online]. Rome. 
https://www.fao.org/fishery/en/collection/area
