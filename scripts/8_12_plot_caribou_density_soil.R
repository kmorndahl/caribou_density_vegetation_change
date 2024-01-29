######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots soil nitrogen across caribou spatial density levels

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/soil_data/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Packages ----------------------------------------------------------------------------

library(tidyverse)

# 1.2 Read in data ----------------------------------------------------------------------------

autumn = read.csv(paste0(inPath, 'density_soil_autumn.csv'))
calving = read.csv(paste0(inPath, 'density_soil_calving.csv'))
postcalving = read.csv(paste0(inPath, 'density_soil_postcalving.csv'))
precalving = read.csv(paste0(inPath, 'density_soil_precalving.csv'))
rut = read.csv(paste0(inPath, 'density_soil_rut.csv'))
summer = read.csv(paste0(inPath, 'density_soil_summer.csv'))
winter = read.csv(paste0(inPath, 'density_soil_winter.csv'))
overall = read.csv(paste0(inPath, 'density_soil_overall.csv'))

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

# Remove unnecessary columns and NAs
df = subset(df, select = -c(.geo, system.index))
df = na.omit(df)

# Tidy property names
names(df) = gsub(pattern = 'mean_p', replacement = 'p', x = names(df))
names(df) = gsub(pattern = 'mean_mean', replacement = 'mean', x = names(df))
names(df) = gsub(pattern = 'mean_stdDev', replacement = 'stdDev', x = names(df))
names(df) = gsub(pattern = '[.]', replacement = '-', x = names(df))
names(df) = gsub(pattern = '_', replacement = '-', x = names(df))
names(df) = gsub(pattern = '-p', replacement = '_p', x = names(df))
names(df) = gsub(pattern = '-mean', replacement = '_mean', x = names(df))
names(df) = gsub(pattern = '-stdDev', replacement = '_stdDev', x = names(df))
names(df) = gsub(pattern = 'caribou-density', replacement = 'caribou_density', x = names(df))

# Pivot longer
df = data.frame(pivot_longer(df, !c(season, caribou_density), 
                             names_pattern = "(.*)_(.*)", 
                             names_to = c("soil_property", ".value")))

# Get list of soil properties
soil_properties = unique(df$soil_property)

# Subset soil properties
soil_properties = grep('0-5cm|5-15cm|15-30cm', soil_properties, value = TRUE)

# Assign y-axis labels for each soil property
y_labels = c('Total Nitrogen, 0-5 cm (g/kg)',
             'Total Nitrogen, 15-30 cm (g/kg)',
             'Total Nitrogen, 5-15 cm (g/kg)')
lookup = data.frame(soil_property = soil_properties, y_label = y_labels, fig_name = c('fig_19_', 'fig_s20_', 'fig_s21_'))

# Join labels
df = inner_join(df, lookup, by='soil_property')

######################################################################################################
######################################################################################################

# 3. PLOT ----------------------------------------------------------------------------

for(soil_property in soil_properties){
  
  fig_name = lookup$fig_name[lookup$soil_property == soil_property]
  outName = paste0(fig_name, 'caribou_density_soil_', soil_property, '.jpg')

  # Subset data
  df_soil_prop = df[df$soil_property == soil_property,]
  
  # Get y label
  y_label = unique(df_soil_prop$y_label)
  
  # Plot
  plt = ggplot(df_soil_prop, aes(x = as.factor(caribou_density)))+
    geom_boxplot(
      aes(ymin = p0, lower = p25, middle = p50, upper = p75, ymax = p100),
      stat = "identity"
    )+
    geom_point(aes(y = mean), size = 4)+
    facet_wrap(~season)+
    theme_minimal(base_size = 40)+
    labs(y = y_label, x = "Caribou relative spatial density")
  plt
  
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
  
}



