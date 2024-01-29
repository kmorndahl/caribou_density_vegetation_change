######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script bins population weighted seasonal occurrence distributions
# Uses population weighted seasonal occurrence distributions calculated from all VHF and GPS data combined

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/range_data/ods/1_season/1_population_weighted/'

outPath = 'data/range_data/ods/1_season/2_binned/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Parameters ----------------------------------------------------------------------------

# Set threshold - SSF values below this threshold are considered caribou absences
threshold = 0.01

# Set number of bins
n = 3

# 1.2 Packages ----------------------------------------------------------------------------

library(terra)
library(sf)
library(classInt)

# 1.3 Read in data ----------------------------------------------------------------------------

# Get occurrence distributions
od_files = list.files(inPath, pattern = "*.tif$", full.names = TRUE)
print("The files are:")
print(od_files)
cat("\n")

# Read in files to raster
# Only read in files that do NOT have the 'vhf' identifier
ods = lapply(grep('vhf', od_files, invert = TRUE, value = TRUE), terra::rast)
print("The rasters are:")
print(ods)
cat("\n")

# Read in step selection function (SSF) outline
ssf = sf::st_read('data/range_data/ssf_outline.shp')

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# Transform SSF to match ODs
ssf = sf::st_transform(ssf, crs = crs(ods[[1]]))

# Extend od to ssf extent
for(i in seq_along(ods)){
  od = ods[[i]]
  od = terra::extend(od, ext(ssf))
  od[is.na(od)] = 0 
  od = terra::mask(od, vect(ssf))
  ods[[i]] = od
}

######################################################################################################
######################################################################################################

# 3. BIN ----------------------------------------------------------------------------

# Pixels lower than the threshold are labeled NA and excluded from binning
# These are functionally 'zeros' i.e. areas where caribou are assumed to be entirely absent
# These will make up the 'None' class

# The rest of the data is binned into 3 bins using the equal interval method

# Set up dataframe for bin metadata
bin_metadata = data.frame()

# Loop over seasonal occurrence distributions
for(od in ods){
  
  # Initialize rasters
  od_zero = od
  od_nz = od
  
  # Get metadata
  name = names(od)[1]
  print(paste0('Name: ', name))
  season = strsplit(name, '_')[[1]][2]

  # Convert anything less than 0.01 to NA
  od_nz[od_nz < threshold] = NA
  
  # Create bins
  bins = classIntervals(values(od_nz), n=n, style="equal")
  print('Intervals created')
  cat("\n")
  
  # Format bins into dataframe
  df.rcl = data.frame(start = bins$brks[1:(length(bins$brks)-1)], 
                      end = bins$brks[2:(length(bins$brks))],
                      class = seq(1,length(bins$brks)-1,1))
  
  # Reclassify raster
  od_bin = terra::classify(od_nz, df.rcl, include.lowest=TRUE)
  print('Raster reclassified')
  cat("\n")
  
  # For zeros raster, convert anything greater than or equal to 
  od_zero[od_zero >= threshold] = NA
  od_zero[od_zero < threshold] = 0
  
  # Mosaic 
  od_final = terra::mosaic(od_bin, od_zero)
  print('Rasters mosaicked')
  cat("\n")
  
  # Add one to avoid issues with zeros and masking
  od_final = od_final + 1
  
  # Format output path
  outName = paste0('od_', season, '_binned.tif')
  
  print(paste0('Output file: ', outName))
  cat("\n")
  
  # Save
  terra::writeRaster(od_final, paste0(outPath, outName), overwrite=TRUE)
  print('Raster saved')
  cat("\n")
  
  # Generate bin metadata
  df.none = data.frame(start = 0, end = df.rcl$start[df.rcl$class == 1], class = 0) # Create 'none' class
  df.out = rbind(df.none, df.rcl) # Bind none class to bin interval dataframe
  df.out$season = season # Assign season
  class_freq = data.frame(freq(od_final)) # Get number of pixels per bin
  df.out$n_obs = class_freq$count # Get number of pixels per bin to output dataframe
  bin_metadata = rbind(bin_metadata, df.out) # Append current season metadata
  
}

write.csv(x = bin_metadata, file = paste0(outPath, 'bin_metadata.csv'), row.names = FALSE)
