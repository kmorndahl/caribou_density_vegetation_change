################################################################################
################################################################################

# CODE DESCRIPTION

# This script takes all individual season-year isopleths and merges them into a single shapefile

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

# NOTE:

# Caribou season-year isopleths are sensitive and cannot be shared publicly
# To request access to season-year isopleths, contact:

# Torsten Bentzen, Wildlife Biologist
# Division of Wildlife Conservation
# Alaska Department of Fish and Game
# torsten.bentzen@alaska.gov

# And request the following data:

# Season-year isopleths for Fortymile caribou fitting the following criteria:
#   - Years: 1991-2021
#   - Seasons: all seasons
#   - Isopleth levels: 0.95, 0.5 

################################################################################
################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/range_data/isopleths/0_season_year/0_orig/'

outPath = 'data/range_data/isopleths/0_season_year/0_orig/'

################################################################################
################################################################################

# 1. SET UP --------------------------------------------------------------------

# 1.1 Set parameters -----------------------------------------------------------

iso_level = 0.95

# 1.2 Packages -----------------------------------------------------------------

library(sf)
library(tidyverse)
library(units)

# 1.3 Read in isopleth data ----------------------------------------------------

# Get all isopleth file names
file_list = list.files(inPath, pattern = "*shp", full.names = TRUE)

# Read in
shp_list = lapply(file_list, read_sf)

################################################################################
################################################################################

# 2. COMBINE ISOPLETHS ---------------------------------------------------------

# Loop through isopleths
for(i in seq_along(file_list)){

  # Get items
  file = file_list[i]
  shp = shp_list[[i]]

  # Get metadata
  name = tail(strsplit(strsplit(tail(strsplit(file, '/')[[1]], n = 1), '[.]')[[1]][1], '_')[[1]], 3)
  type = name[1]
  season = name[2]
  year = name[3]

  # Set metadata
  shp$type = type
  shp$season = season
  shp$year = year

  # Calculate area
  shp$area = sf::st_area(shp)
  shp$area_km = units::set_units(shp$area, "km^2")

  # Replace feature
  shp_list[[i]] = shp
  
}

# Combine into one shapefile
shp = do.call(what = sf:::rbind.sf, args=shp_list)

################################################################################
################################################################################

# 3. SAVE ----------------------------------------------------------------------

st_write(shp, paste0(outPath, 'iso_all.shp'))
write.csv(st_drop_geometry(shp), paste0(outPath, 'iso_all.csv'), row.names = FALSE)


