################################################################################
################################################################################

# CODE DESCRIPTION

# This script uses caribou VHF collar data to produce annual, seasonal isopleths and occurrence distributions

# CODE AUTHOR: Kathleen Orndahl, adapted from code by Eric Palm
# CONTACT: kathleen.orndahl@nau.edu

# RESOURCES:
# https://peerj.com/articles/11031.pdf

# NOTE:

# Caribou collar data are sensitive and cannot be shared publicly
# To request access to caribou collar data, contact:

# Torsten Bentzen, Wildlife Biologist
# Division of Wildlife Conservation
# Alaska Department of Fish and Game
# torsten.bentzen@alaska.gov

# And request the following data:

# GPS and VHF collar locations for Fortymile caribou fitting the following criteria:
#   - Years: 1991-2021
#   - Seasons: all seasons
#   - Resolution: all available data
#   - Cohort: all cohorts

################################################################################
################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/collar_data/'

outPathIso = 'data/range_data/isopleths/season_year/0_orig/'
outPathOD = 'data/range_data/ods/season_year/0_orig/'

################################################################################
################################################################################

# 1. SET UP --------------------------------------------------------------------

# 1.1 Set parameters -----------------------------------------------------------

# current_season = params[1] # OPTIONAL: Load season from bash script, allows seasons to be run in parallel to reduce compute time
h_method = amt::hr_kde_ref # Bandwidth calculation method
t_rast_res = 3000 # Template raster resolution

# 1.2 Packages -----------------------------------------------------------------

# All packages except 'terra' were installed from CRAN using install.packages()
# 'terra' development version was installed using remotes::install_github("rspatial/terra")
# Package versions: terra_1.3-10, amt_0.1.4
# Use sessionInfo() to get package versions on your machine
# Most of the heavy lifting here is done by 'amt' (animal movement tools) and the 'ctmm'
# packages, which 'amt' calls do estimate occurrence distributions

require(lubridate)
require(amt)
require(terra)
require(tidyr)
require(dplyr)
require(sf)

# 'amt' package plays nicer with the older version of nest and unnest functions from 'tidyr' package.
nest = nest_legacy
unnest = unnest_legacy

# 1.3 Functions ----------------------------------------------------------------

# Function to retrieve bandwidth calculation
# Output from hr_kde_lscv is a named list rather that a vector, we need to select just the $h slot
h_calc = function(trk){
  h = h_method(trk)
  if(h_method_name == 'lscv'){
    h = h$h
  }
  return(h)
}

# Functions to normalize raster between 0 and 1.
norm_raster = function (x) {(x-x@data@min)/(x@data@max-x@data@min)}
norm_terra = function (x) {(x-minmax(x)[1])/(minmax(x)[2]-minmax(x)[1])}

# 1.4 Read in VHF collar data --------------------------------------------------

fmch_raw = read.csv(paste0(inPath, 'fmch_vhf_data.csv'))

################################################################################
################################################################################

# 2. PREPARE DATA --------------------------------------------------------------

# 2.1 Tidy VHF collar data -----------------------------------------------------

# Select necessary columns only
fmch_raw = dplyr::select(fmch_raw, c(Year, Season, POINT_X, POINT_Y))

# Tidy season names to match GPS data
fmch_raw$Season = tolower(fmch_raw$Season)
fmch_raw$Season = gsub('fallmigration', 'autumn', fmch_raw$Season)
fmch_raw$Season = gsub('rutearlywinter', 'rut', fmch_raw$Season)

# Turn into 'amt' style track
# NOTE: point field lat/long data are in NAD 83 Yukon Albers
fmch_trk = amt::mk_track(fmch_raw, .x = POINT_X, .y = POINT_Y, year = Year, season = Season, crs =  sp::CRS("+init=epsg:3578"))

# 2.2 Prepare template raster --------------------------------------------------

#' Make a single template raster for creating occurrence distributions so they can easily
#' be merged together later. Choose whatever spatial resolution you want. The factor argument 
#' adds a buffer around the GPS location extent so the 'ctmm' package (which is called by 'amt'), 
#' doesn't bump up against the edge of the extent when trying to create the occurrence distributions.
#' A larger factor value takes more time to run, but ensures the ODs aren't cut off.
#' This is a trial and error decision. If units in your coordinate reference system are
#' meters, then the resolution here is in meters too.

t_rast = amt::make_trast(fmch_trk, res = t_rast_res, factor = 1.5)

################################################################################
################################################################################

# 3. CALCULATE SEASONAL ISOPLETHS AND KDEs -------------------------------------

# 3.1 Prepare collar data ------------------------------------------------------

#' Walk-through of the following code:
#' 1. Nests the data frame so each season-year combination has one row
#'    and the telemetry data becomes a list-column called data, to which we can
#'    apply functions
#' 2. Create season_year column to track unique combinations of season and year
#' 3. Calculate bandwidth
#' 4. Create kde column, calculate and store KDE results
#' 5. Create iso column, calulate isopleths using KDE and store as sf 
#' 6. Create od column, store OD results
#' 7. Convert from 'raster' to 'terra' to facilitate computation
#' 8. Create od_norm column, store normalized OD results

season_year_iso_od =
  fmch_trk %>%
  tidyr::nest(data = c(x_, y_)) %>% 
  dplyr::mutate(
    season_year = as.factor(paste(season, year, sep = "_")),
    n = purrr::map(data, nrow),
    h = purrr::map(data, h_calc),
    kde = purrr::map2(.x = data, .y = h, .f = ~amt::hr_kde(.x, levels = c(0.5, 0.9, 0.95), h = .y, trast = t_rast)),
    iso = purrr::map(kde, ~amt::hr_isopleths(., levels = c(0.5, 0.9, 0.95))),
    od = purrr::map(kde, amt::hr_ud),
    od_terra = purrr::map(od, terra::rast),
    od_norm = purrr::map(od_terra, norm_terra)
  )

print('Annual, seasonal isopleths and occurrence distributions created:')
print(season_year_iso_od)
cat('\n')

################################################################################
################################################################################

# 4. SAVE SEASON-YEAR DATA -----------------------------------------------------

# 4.1 Prepare season-year metadata ---------------------------------------------

# Get list of season-year combinations
season_years = levels(season_year_iso_od$season_year)

print('The season/years are:')
print(season_years)
cat('\n')

# 4.2 Isopleths ----------------------------------------------------------------

#' Walk-through of the following code...
#' For each season-year:
#' 1. Get isopleths
#' 2. Save

for (i in seq_along(season_years)){
  out_iso = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$iso[[1]]
  out_h = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$h[[1]][1]
  out_n = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$n[[1]][1]
  out_iso$bandwidth = out_h
  out_iso$n = out_n
  sf::st_write(out_iso, dsn=paste0(outPathIso, 'iso_vhf_', season_years[i], ".shp"), delete_layer = TRUE)
}

print('Annual, seasonal isopleths created and saved')
cat('\n')

# 4.3 ODs ----------------------------------------------------------------------

# Create list of ODs
od_list = list()
for (i in seq_along(season_years)){
  od_list[[i]] = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$od_norm[[1]]
  names(od_list[[i]]) = paste0("od_vhf_", season_years[i])
}

print('Annual, seasonal occurrence distributions aggregated:')
print(od_list)
cat('\n')

# Save rasters
purrr::map(od_list, ~terra::writeRaster(., filename=paste0(outPathOD, names(.), ".tif"), filetype = "GTiff", overwrite = TRUE))

print('Annual, seasonal occurrence distributions written to raster')
cat('\n')

