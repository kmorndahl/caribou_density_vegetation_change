######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots Fortymile caribou population size time series

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/misc_data/'

outPath = 'figures/'

outName = 'fig_1_annual_caribou_population.jpg'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Packages ----------------------------------------------------------------------------

library(tidyverse)
library(scales)

# 1.2 Read in and tidy data ----------------------------------------------------------------------------

# Read in population data
pop_size = read.csv(paste0(inPath, 'fmch_population_size.csv'))

# Working copy
data = pop_size

# Filter to only photocensus data
data = data[(data$estimate_type == 'photocensus'|data$estimate_type == 'model'),]

# Remove non-Rivest data for later years
data = data[!(data$year>=2009 & data$rivest_adjustment == 'N'),]

######################################################################################################
######################################################################################################

# 2. PLOT --------------------------------------------------------------------------------------------

plt = ggplot(data, aes(x = year, y = population_size))+
  annotate("rect", xmin = 2009, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .05)+
  geom_smooth(col = 'grey50', fill = 'grey50', alpha = 0.3)+
  geom_point(size = 6, aes(shape = estimate_type))+
  labs(x = 'Year', y = 'Caribou Population Size', shape = '')+
  theme_minimal()+
  theme(text = element_text(size = 40),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position="top")+
  scale_y_continuous(label=comma)+
  scale_shape_manual(values=c(17, 19))

plt

######################################################################################################
######################################################################################################

# 3. SAVE --------------------------------------------------------------------------------------------

ggsave(
  paste0(outPath, outName),
  plt,
  width = 30,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)
