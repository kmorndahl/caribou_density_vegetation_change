######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script calculates overlap between each seasonal range and the Core Uplands

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

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPathRanges = 'data/range_data/isopleths/0_season_year/0_orig/'
inPathCore = 'data/range_data/isopleths/1_season/1_population_weighted/'

outPath = 'data/range_data/isopleths/0_season_year/0_orig/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Set parameters ----------------------------------------------------------------------------

# Set season used for determining 'Core Uplands'
core_season = 'overall'

# Set isopleth used for determining 'Core Uplands'
core_iso = 0.5

# 1.2 Packages ----------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(units)
library(smoothr)

# 1.3 Read in data ----------------------------------------------------------------------------

iso_init = read_sf(paste0(inPathRanges, 'iso_all.shp'))
core = read_sf(paste0(inPathCore, 'iso_seasons_population_weighted_vhf.shp'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# Select appropriate range to use for 'Core Uplands'
core = core[(core$season == core_season & core$isopleth == core_iso),]

# Reproject to match season-year range isopleths
core = st_transform(core, st_crs(ranges))

# Smooth Core Uplands
core = smoothr::drop_crumbs(core, units::set_units(100000000, m^2))
core = smoothr::smooth(core_smooth, method = "ksmooth", smoothness = 2)

# Create copy
iso = iso_init

# Add intersection field
iso$intersection_area = NA

######################################################################################################
######################################################################################################

# 3. CALCULATE RANGE OVERLAP ----------------------------------------------------------------------------

# Loop through features and calculate intersection
for(i in 1:nrow(iso)){
  feat = iso[i,]
  intersection = st_intersection(st_geometry(feat), st_geometry(core))
  int_area = st_area(intersection)
  if(length(int_area) == 0){
    iso[i,]$intersection_area = set_units(0, m^2)
  }else{
    iso[i,]$intersection_area = int_area
  }
}

# Convert to km2
iso$intersection_area_km = units::set_units(iso$intersection_area, "km^2")

# Rename columns for shp
iso = iso %>% dplyr::rename(int_m2 = intersection_area, int_km2 = intersection_area_km)

######################################################################################################
######################################################################################################

# 4. SAVE ----------------------------------------------------------------------------

# Save
st_write(iso, paste0(outPath, 'iso_all_overlap.shp'), delete_layer = TRUE)
write.csv(st_drop_geometry(iso), paste0(outPath, 'iso_all_overlap.csv'), row.names = FALSE)

