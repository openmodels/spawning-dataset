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
   
### If you want to reproduce the Spawning ProCreator spreadsheet

1. Geocode spawning region names: Set `source = 'arcgis'` in
   `code/prelim/geocode.py` and run the script; then set `source =
   'geonames'` and run the script again. This script produces geocoded
   result files names `localities-arcgis.csv` and
   `localities-geonames.csv`. Move these to the `inputs/` directory.

2. Run the `code/prelim/spawning-geoprep.R` script, which constructs
   the raw Spawning ProCreator spreadsheet into
   `outputs/master.csv`. This can then be imported into Excel or
   Google Sheets for filling out the Verdict column.
