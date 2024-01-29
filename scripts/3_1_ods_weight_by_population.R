######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script weights season-year occurrence distributions by population size
# We do this separately for:
# VHF data from the period 1991-2008
#   - This used to calculate the 'Core Uplands'
#   - Range expansion is observed starting after this period
# All VHF and GPS data combined
#   - This is used for all other analyses

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

# NOTE:

# Caribou season-year occurrence distributions are sensitive and cannot be shared publicly
# To request access to season-year occurrence distributions, contact:

# Torsten Bentzen, Wildlife Biologist
# Division of Wildlife Conservation
# Alaska Department of Fish and Game
# torsten.bentzen@alaska.gov

# And request the following data:

# Season-year occurrence distributions for Fortymile caribou fitting the following criteria:
#   - Years: 1991-2021
#   - Seasons: all seasons

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/range_data/ods/0_season_year/0_orig/'

outPath = 'data/range_data/ods/1_season/1_population_weighted/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Parameters ----------------------------------------------------------------------------

# Set start and end year for ODs used to identify 'Core Uplands'
vhf_start = 1991
vhf_end = 2008

# Set list of dataset types to loop through
ds_types = c('vhf', '')

# 1.2 Packages ----------------------------------------------------------------------------

library(terra)
library(sf)

# 1.3 Functions ----------------------------------------------------------------------------

# Functions to normalize raster between 0 and 1
norm_terra <- function (x) {(x-minmax(x)[1])/(minmax(x)[2]-minmax(x)[1])}
norm_raster <- function (x) {(x-x@data@min)/(x@data@max-x@data@min)}

# 1.4 Read in data ----------------------------------------------------------------------------

# Get in occurrence distribution file names
od_files = list.files(inPath, pattern = "*.tif$", full.names = TRUE)
print("The files are:")
print(od_files)
cat("\n")

# Remove any 2021 ODs, we do not have population information for 2021
od_files = od_files[lapply(od_files,function(x) length(grep("2021",x,value=FALSE))) == 0]

# Get population data
population = read.csv('data/misc_data/fmch_population_size.csv')
print("The population data is:")
print(population)
cat("\n")

######################################################################################################
######################################################################################################

# 2. TIDY DATA ----------------------------------------------------------------------------

# 2.1 OD data ----------------------------------------------------------------------------

# Get all season/year combinations
season_year_df = data.frame()
for(i in seq_along(od_files)){
  file = od_files[i]
  name = tail(strsplit(file, '/')[[1]], 1)
  name = gsub('od_gps_', '', name)
  name = gsub('od_vhf_', '', name)
  name = gsub('.tif', '', name)
  name_df = data.frame(season_year = name)
  name_df = name_df %>% separate(season_year, c("season", "year"))
  season_year_df = rbind(season_year_df, name_df)
}
season_year_df$year = as.numeric(season_year_df$year)

# 2.2 Population data ----------------------------------------------------------------------------

# Remove repetitive data
population = population[!(population$year>=2009 & population$rivest_adjustment == 'N'),] # Remove non-Rivest data for later years
population = population[!(population$year==2009 & population$estimate_type == 'model'),] # Remove modeled estimates for years where there is a photocensus
population = population[!(population$year==2017 & population$estimate_type == 'model'),] # Remove modeled estimates for years where there is a photocensus
population = population[!(population$year==2007 & population$estimate_type == 'photocensus'),] # Remove modeled estimates for years where there is a photocensus

# Add population data
season_population = dplyr::left_join(season_year_df, dplyr::select(population, c(year, population_size)), by = c('year' = 'year'))

# Summarize seasonal population data
season_population_summary_init = season_population %>%
  group_by(season) %>%
  mutate(population_total = sum(population_size))

######################################################################################################
######################################################################################################

# 3. WEIGHT ODs BY POPULATION SIZE ----------------------------------------------------------------------------

# - For each season-year, multiply the OD raster by the population in that year
# - Then, divide this raster by the total population, summed across all years
#   - NOTE: the total population will be different for each season because 
#           it sums only the years with data for that season
# - od_weighted = (OD * annual population) / total population

for(ds_type in ds_types){ # START DATASET TYPE LOOP

  # 3.1 Read in and subset data ----------------------------------------------------------------------------
  
  if(ds_type == 'vhf'){
    
    # Subset population data
    season_population_summary = season_population_summary_init[season_population_summary_init$year <= vhf_end,]
    
    # Subset OD files and read in to raster
    ods = lapply(grep('vhf', od_files, value = TRUE), terra::rast)
    print("The rasters are:")
    print(ods)
    cat("\n")
    
  }else{
    
    # Use all population data
    season_population_summary = season_population_summary_init
    
    # Read in OD files to raster
    ods = lapply(od_files, terra::rast)
    print("The rasters are:")
    print(ods)
    cat("\n")
    
  }
  
  # 3.2 Initialize tibble and lists to store weighted ODs ----------------------------------------------------------------------------
  
  ods_xyrpop = tibble(
    year = 1:length(ods),
    season = 'temp',
    od_xyrpop = list(ods[[1]])
  )
  
  ods_weighted <- list()
  
  # 3.3 Multiply ODs by annual population size ----------------------------------------------------------------------------
  
  for (i in seq_along(ods)){
    
    # Get OD
    od = ods[[i]]
    
    # Get metadata from ODs
    year = as.numeric(tail(strsplit(names(od), '_')[[1]], 1))
    season = head(tail(strsplit(names(od), '_')[[1]], 2), 1)
    
    # Get population sizes
    yr_pop = season_population_summary$population_size[season_population_summary$year == year & season_population_summary$season == season]
    
    # Multiply OD by annual population
    od_xyrpop = od * yr_pop
    
    # Save weighted OD to dataframe
    ods_xyrpop$year[i] = year
    ods_xyrpop$season[i] = season
    ods_xyrpop$od_xyrpop[[i]] = od_xyrpop
    
    print(paste0('Raster ', outName, ' written to disk'))
    cat("\n")
    
  }
  
  # 3.4 Divide ODs by total seasonal population size ----------------------------------------------------------------------------
  
  # Get list of seasons
  seasons = unique(season_population_summary$season)
  
  print('The seasons are:')
  print(seasons)
  cat('\n')
  
  for(i in seq_along(seasons)){
    
    # Get seasonal population total (population summed across years)
    season = seasons[[i]]
    total_pop = season_population_summary$population_total[season_population_summary$season == season][1]
    
    # Get all ODs for the specified season and stack
    # Average ODs
    # Divide by total seasonal population size
    # Normalize to [0, 1]
    ods_weighted[[i]] <- ods_xyrpop$od_xyrpop[ods_xyrpop$season == seasons[i]] %>% 
      terra::src(.) %>%
      terra::mosaic(fun = "mean") %>%
      `/`(total_pop) %>%
      norm_terra(.)
    
  }
  
  # 3.5 Finalize population weighted ODs ----------------------------------------------------------------------------
  
  # Calculate 'overall' season population weighted OD
  # Get all population weighted ODs and stack
  # Average 
  # Normalize to [0, 1]
  od_overall = terra::src(ods_weighted) %>%
    terra::mosaic(fun = "mean") %>%
    norm_terra(.)
  names(od_overall) = 'od_gps_overall_2011' # Temporary name, names are tidied later
  
  ods_all = c(ods_weighted, od_overall)
  
  ######################################################################################################
  ######################################################################################################
  
  # 4. SAVE ----------------------------------------------------------------------------
  
  for(i in seq_along(ods_all)){
    
    # Tidy raster name to reflect seasonal aggregation
    od = ods_all[[i]]
    new_name = paste(head(strsplit(names(od), '_')[[1]], -1), collapse = '_') # Remove year
    new_name = gsub('gps_|vhf_', '', new_name) # Remove gps/vhf
    names(od) = new_name
    ods_all[[i]] = od
    
    if(ds_type == 'vhf'){
      outName = paste0(new_name, '_population_weighted_', ds_type, '.tif')
    }else{
      outName = paste0(names(od), '_population_weighted.tif')
    }
    terra::writeRaster(od, filename = paste0(outPath, outName), filetype = "GTiff")
    print(paste0('Raster ', outName, ' written to disk'))
    cat("\n")
    
  }

} # END DATASET TYPE LOOP