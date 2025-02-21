################################################################################
################################################################################

# CODE DESCRIPTION

# This script uses caribou GPS collar data to produce annual, seasonal isopleths and occurrence distributions

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

# To run all seasons at once, specify 'all'
# Otherwise, specify season which when run on HPC allows seasons to be run in parallel to reduce compute time
current_season = 'all'

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

# Functions to normalize raster between 0 and 1
norm_raster = function (x) {(x-x@data@min)/(x@data@max-x@data@min)}
norm_terra = function (x) {(x-minmax(x)[1])/(minmax(x)[2]-minmax(x)[1])}

# 1.4 Load GPS collar data -----------------------------------------------------

fmch_raw = readRDS(paste0(inPath, 'fmch_trk.rds'))

################################################################################
################################################################################

# 2. PREPARE DATA --------------------------------------------------------------

# 2.1 Prepare CRS --------------------------------------------------------------

# Work around to creating a CRS that works with the older version of amt
# Get CRS from .rds file -- ABoVE CRS, ESRI:102001
proj = get_crs(fmch_raw)@projargs
sp::CRS(proj)

# Must specify CRS using espg code otherwise it trips up later code (amt version > 1.0)
fmch_trk = amt::mk_track(fmch_raw, .x = x_, .y = y_, .t = t_, id = id, crs = sp::CRS(proj))

print('The track created with appropriate CRS:')
print(fmch_trk)
cat('\n')

# 2.2 Prepare template raster --------------------------------------------------

#' Make a single template raster for creating occurrence distributions so they can easily
#' be merged together later. Choose whatever spatial resolution you want. The factor argument 
#' adds a buffer around the GPS location extent so the 'ctmm' package (which is called by 'amt'), 
#' doesn't bump up against the edge of the extent when trying to create the occurrence distributions.
#' A larger factor value takes more time to run, but ensures the ODs aren't cut off.
#' This is a trial and error decision. If units in your coordinate reference system are
#' meters, then the resolution here is in meters too.

t_rast = amt::make_trast(fmch_trk, res = t_rast_res, factor = 1.5)

print('Template raster created:')
print(t_rast)
cat('\n')

################################################################################
################################################################################

# 3. CALCULATE SEASONAL ISOPLETHS AND KDEs -------------------------------------

# 3.1 Prepare collar data ------------------------------------------------------

#' Walk-through of the following code:
#' 1. Adds 'year' column and makes anything before December 1 the next year
#'    because we chose December 1 -- onset of winter -- as the start of the year
#' 2. Adds 'season' column and assigns seasons based on Boertje et al. 2012
#' 3. Adds a 'season_length' column that calculates each season's length in days
#' 4. Nests the data frame so each id-year-season combination has one row
#'    and the telemetry data becomes a list-column called data, to which we can
#'    apply functions
#' 5. Adds 'steps' list-column where each id-year-season track is filtered -- 
#'    or regularized -- so there is one location every five hours, with a 55 
#'    minute tolerance. These are user decisions. We chose 5 hours because it 
#'    was the finest temporal resolution for many animals.
#' 6. Adds 'n' column which gives the number of locations for each id-year.
#' 7. Adds 'duration' column to calculate how long (in days) each animal was 
#'    marked during a particular year.
#' 8. Unnests the 'duration' and 'n' columns.
#' 9. Only retain animals that were active for over two thirds of that 
#'    season-year and those that had at least twice as many locations as number
#'    of days active. These are user-specific decisions.
#' 10. Unnests the steps
#' 11. Re-nests the steps by just season and year
#' 12. Converts the steps back into a track

season_year_trk_df =
  fmch_trk %>%
  mutate(year = if_else(yday(t_) >= 335, year(t_)+1, year(t_)),
         season = case_when(yday(t_) >= 91 & yday(t_) < 131 ~ "precalving",
                            yday(t_) >= 131 & yday(t_) < 148 ~ "calving",
                            yday(t_) >= 148 & yday(t_) < 182 ~ "postcalving",
                            yday(t_) >= 182 & yday(t_) < 228 ~ "summer",
                            yday(t_) >= 228 & yday(t_) < 274 ~ "autumn",
                            yday(t_) >= 274 & yday(t_) < 335 ~ "rut",
                            TRUE ~ "winter")) %>%
  group_by(year, season) %>%
  mutate(season_length = round(((difftime(max(t_), min(t_), units="days"))))) %>%
  ungroup() %>%
  nest(-id, -year, -season, -season_length) %>%
  mutate(steps = map(data, ~track_resample(., rate = hours(5), tolerance = minutes(55))),
         n = map(steps, nrow),
         duration = map(steps, ~round(((difftime(max(.$t_), min(.$t_), units="days")))))) %>%
  unnest(duration, n) %>%
  filter(as.numeric(duration)/as.numeric(season_length) > 0.66,
         n/as.numeric(season_length) >= 2) %>%
  unnest(steps) %>%
  select(c(year, season, x_, y_, t_)) %>%
  nest(-year, -season) %>%
  mutate(data = purrr::map(data, ~amt::mk_track(., .x = x_, .y = y_, .t = t_, crs = sp::CRS(proj))))

print('Data prepped and grouped by season/year:')
print(season_year_trk_df)
cat('\n')

# Convert seasons to one word
season_year_trk_df$season = gsub('autumn_migration', 'autumn', season_year_trk_df$season)
season_year_trk_df$season = gsub('rut_early_winter', 'rut', season_year_trk_df$season)
season_year_trk_df$season = gsub('pre_calving', 'precalving', season_year_trk_df$season)
season_year_trk_df$season = gsub('post_calving', 'postcalving', season_year_trk_df$season)

# Subset season if specified
if(current_season != 'all'){
  season_year_trk_df = season_year_trk_df[season_year_trk_df$season == current_season,]
}

# 3.2 Create isopleths and KDEs ------------------------------------------------

#' Walk-through of the following code...
#' Data is nested at the season-year level, so for each season-year:
#' 1. Create season_year column to track unique combinations of season and year
#' 2. Calculate bandwidth
#' 3. Create kde column, calculate and store KDE results
#' 4. Create iso column, calulate isopleths using KDE and store as sf 
#' 5. Create od column, store OD results
#' 6. Convert from 'raster' to 'terra' to facilitate computation
#' 7. Create od_norm column, store normalized OD results

season_year_iso_od =
  season_year_trk_df %>%
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
#' 2. Convert to Yukon Albers, a well behaved CRS
#' 3. Save

for (i in seq_along(season_years)){
  out_iso = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$iso[[1]]
  out_iso = st_transform(out_iso, sp::CRS("+init=epsg:3578"))
  out_h = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$h[[1]][1]
  out_n = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$n[[1]][1]
  out_iso$bandwidth = out_h
  out_iso$n = out_n
  sf::st_write(out_iso, dsn=paste0(outPathIso, 'iso_gps_', season_years[i], ".shp"), delete_layer = TRUE)
}

print('Annual, seasonal isopleths created and saved')
cat('\n')

# 4.3 ODs ----------------------------------------------------------------------

# Create list of ODs
od_list = list()
for (i in seq_along(season_years)){
  od_list[[i]] = season_year_iso_od[season_year_iso_od$season_year == season_years[i],]$od_norm[[1]]
  names(od_list[[i]]) = paste0("od_gps_", season_years[i])
}

print('Annual, seasonal occurrence distributions aggregated:')
print(od_list)
cat('\n')

# Save rasters
purrr::map(od_list, ~terra::writeRaster(., filename=paste0(outPathOD, names(.), ".tif"), filetype = "GTiff", overwrite = TRUE))

print('Annual, seasonal occurrence distributions written to raster')
cat('\n')


