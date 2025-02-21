######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots sample size and bandwidth for kernel density estimator generation

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/range_data/isopleths/0_season_year/0_orig/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Parameters ----------------------------------------------------------------------------

# Set isopleth used for season-year ranges
iso_level = 0.95

# Set scaling coefficient to use for double axis graph
coeff = 0.5

# Set color palette
colorblind = c("#009E73", "#E69F00", "#999999")

# 1.2 Packages ----------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(units)

# 1.3 Read in data ----------------------------------------------------------------------------

iso = read.csv(paste0(inPath, 'iso_all_overlap.csv'))

######################################################################################################
######################################################################################################

# 2. PLOT ----------------------------------------------------------------------------

# Select appropriate isopleth for season-year ranges
iso = iso[iso$level == iso_level,]

# Plot
plt = ggplot(iso, aes(x = as.numeric(year)))+
  geom_point(aes(y=bandwidth), col = '#E69F00', size = 3)+
  geom_point(aes(y=n/coeff), col = '#009E73', size = 3)+
  geom_line(aes(y=bandwidth), col = '#E69F00')+
  geom_line(aes(y=n/coeff), col = '#009E73')+
  facet_wrap(~season)+
  labs(x = 'Year')+
  scale_y_continuous(name = "Bandwidth", sec.axis = sec_axis(~.*coeff, name="Sample Size"))+
  theme_minimal(base_size = 24)+
  theme(
    axis.title.y = element_text(color = '#E69F00'),
    axis.title.y.right = element_text(color = '#009E73'),
    axis.text.y = element_text(color = '#E69F00'),
    axis.text.y.right = element_text(color = '#009E73'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust= 1, size = 18),
    axis.text.x.bottom = element_text(margin = margin(-3, -3, -3, -3)),
  )
plt

######################################################################################################
######################################################################################################

# 3. SAVE ----------------------------------------------------------------------------

outName = 'fig_s2_sample_size_bandwidth.jpg'

ggsave(
  paste0(outPath, outName),
  plt,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)



