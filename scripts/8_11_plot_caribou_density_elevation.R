######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots elevation across caribou spatial density levels

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/elevation_data/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Packages ----------------------------------------------------------------------------

library(tidyverse)

# 1.2 Read in data ----------------------------------------------------------------------------

autumn = read.csv(paste0(inPath, 'density_elevation_autumn.csv'))
calving = read.csv(paste0(inPath, 'density_elevation_calving.csv'))
postcalving = read.csv(paste0(inPath, 'density_elevation_postcalving.csv'))
precalving = read.csv(paste0(inPath, 'density_elevation_precalving.csv'))
rut = read.csv(paste0(inPath, 'density_elevation_rut.csv'))
summer = read.csv(paste0(inPath, 'density_elevation_summer.csv'))
winter = read.csv(paste0(inPath, 'density_elevation_winter.csv'))
overall = read.csv(paste0(inPath, 'density_elevation_overall.csv'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# Assign tidy season names
autumn$season = 'Autumn'
calving$season = 'Calving'
postcalving$season = 'Post-calving'
precalving$season = 'Pre-calving'
rut$season = 'Rut'
summer$season = 'Summer'
winter$season = 'Winter'
overall$season = 'Overall'

# Combine data
df = rbind(autumn, calving, postcalving, precalving, rut, summer, winter, overall)

# Tidy
df = subset(df, select = -c(.geo, system.index))
df = na.omit(df)

######################################################################################################
######################################################################################################

# 3. PLOT ----------------------------------------------------------------------------

plt = ggplot(df, aes(x = as.factor(caribou_density)))+
  geom_boxplot(aes(ymin = p0, lower = p25, middle = p50, upper = p75, ymax = p100),
    stat = "identity")+
  geom_point(aes(y = mean), size = 4)+
  facet_wrap(~season)+
  theme_minimal(base_size = 40)+
  labs(y = 'Elevation (m)', x = "Caribou relative spatial density")
plt

######################################################################################################
######################################################################################################

# 4. SAVE ----------------------------------------------------------------------------

outName = 'fig_s18_caribou_density_elevation.jpg'

# Save
ggsave(
  paste0(outPath, outName),
  plt,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)


