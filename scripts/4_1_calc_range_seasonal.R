################################################################################
################################################################################

# CODE DESCRIPTION

# This script creates isopleths from population weighted seasonal occurrence distributions
# We do this separately for:
# VHF data from the period 1991-2008
#   - This used to calculate the 'Core Uplands'
#   - Range expansion is observed starting after this period
# All VHF and GPS data combined
#   - This is used for all other analyses

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

################################################################################
################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/range_data/ods/1_season/1_population_weighted/'

outPath = 'data/range_data/isopleths/1_season/1_population_weighted/'

################################################################################
################################################################################

# 1. SET UP --------------------------------------------------------------------

# 1.1 Parameters ---------------------------------------------------------------

# Set start and end year for ODs used to identify 'Core Uplands'
vhf_start = 1991
vhf_end = 2008

# Set list of dataset types to loop through
ds_types = c('vhf', '')

# Set isopleth values
isopleth_values = c(0.5, 0.7, 0.725, 0.9, 0.95)

# Set projection for rasters
# NAD83 / Yukon Albers (EPSG:3578)
out_crs = "epsg:3578"

# 1.2 Packages -----------------------------------------------------------------

library(terra)
library(sf)

# 1.3 Read in data -------------------------------------------------------------

# Get occurrence distributions
od_files = list.files(inPath, pattern = "*.tif$", full.names = TRUE)
print("The files are:")
print(od_files)
cat("\n")

################################################################################
################################################################################

# 2. CALCULATE ISOPLETHS FROM ODs ----------------------------------------------

for(ds_type in ds_types){ # START DATASET TYPE LOOP

  if(ds_type == 'vhf'){
    
    # Read in files to raster - ODs
    ods = lapply(grep('vhf', od_files, value = TRUE), terra::rast)
    print("The rasters are:")
    print(ods)
    cat("\n")
    
  }else{
    
    # Read in files to raster - ODs
    ods = lapply(grep('vhf', od_files, invert = TRUE, value = TRUE), terra::rast)
    print("The rasters are:")
    print(ods)
    cat("\n")
    
  }
  
  # Set raster CRS
  for(i in seq_along(ods)){
    crs(ods[[i]]) = out_crs
  }
  
  # Initialize isopleth list
  iso_list = list()
  
  # Loop through seasonal ODs
  for(od in ods){
    
    # Loop through isopleth values
    for(val in isopleth_values){
      
      # Get polygon based on isopleth value
      poly = as.polygons(od > (1-val))
      poly = poly[2] # Get only where OD > 1-val is true
      poly$isopleth = val # Set isopleth value attribute
      poly$season = strsplit(names(od), '_')[[1]][2] # Set season attribute
      poly = poly[,-1] # Remove binary column
  
      # Add to isopleth list
      iso_list = c(iso_list, poly)
      
    }
    
  }
  
  # Convert isopleth list to sf dataframe
  iso_sf = do.call(rbind, iso_list)
  iso_sf = sf::st_as_sf(iso_sf)
  
  ##############################################################################
  ##############################################################################
  
  # 3. SAVE --------------------------------------------------------------------
  
  if(ds_type == 'vhf'){
    st_write(iso_sf, paste0(outPath, 'iso_seasons_population_weighted_', ds_type, '.shp'))
  }else{
    st_write(iso_sf, paste0(outPath, 'iso_seasons_population_weighted.shp'))
  }

} # END DATASET TYPE LOOP
