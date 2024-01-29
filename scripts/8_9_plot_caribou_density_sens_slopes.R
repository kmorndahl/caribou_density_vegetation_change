######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots sens slopes of PFT change partitioned by caribou density and season

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/veg_data/sens_slopes/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Parameters ----------------------------------------------------------------------------

# Set season for summaries
selected_season = 'overall'

# Set start and end year
start_year = 1992
end_year = 2020

# Set list of PFTs
pfts = c('Deciduous shrubs', 'Graminoids', 'Lichens')

# Set list of shrubs
shrubs = c('Alder', 'Birch', 'Willow')

# Set p-value threshold
p_value = '005'

# Set number visualization
options(scipen = 999)

# Set palettes
cb_palette_pft = c("#117733", "#CC6677", "#88CCEE", "#332288", "#DDCC77", "#AA4499", 
                   "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

# 1.2 Packages ----------------------------------------------------------------------------

library(tidyverse)
library(stringr)

# 1.3 Read in data ----------------------------------------------------------------------------

autumn = read.csv(paste0(inPath, 'sens_slopes_autumn.csv'))
calving = read.csv(paste0(inPath, 'sens_slopes_calving.csv'))
postcalving = read.csv(paste0(inPath, 'sens_slopes_postcalving.csv'))
precalving = read.csv(paste0(inPath, 'sens_slopes_precalving.csv'))
rut = read.csv(paste0(inPath, 'sens_slopes_rut.csv'))
summer = read.csv(paste0(inPath, 'sens_slopes_summer.csv'))
winter = read.csv(paste0(inPath, 'sens_slopes_winter.csv'))
overall = read.csv(paste0(inPath, 'sens_slopes_overall.csv'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# 2.1 Combine data -----------------------------------------------------------------

slope = rbind(autumn, calving, postcalving, precalving, rut, summer, winter, overall)
slope = dplyr::select(slope, -c(system.index, .geo))

# 2.2 Format data -----------------------------------------------------------------

# Pivot longer
slope_long = pivot_longer(slope, !c(LC, caribou_density, season), names_to = 'pft', values_to = 'slope')
slope_long = separate(slope_long, pft, c(NA, NA, "pft", "metric"))

# Pivot wider
slope_long = pivot_wider(slope_long, names_from = metric, values_from = slope)

# 2.3 Tidy data -----------------------------------------------------------------

# Filter out zero area
slope_long = slope_long[slope_long$count > 0,]

# Land cover names
slope_long$LC = gsub('LC10', 'Sparsely vegetated', slope_long$LC)
slope_long$LC = gsub('LC11', 'Fen', slope_long$LC)
slope_long$LC = gsub('LC12', 'Bog', slope_long$LC)
slope_long$LC = gsub('LC13', 'Shallows/littoral', slope_long$LC)
slope_long$LC = gsub('LC14', 'Barren', slope_long$LC)
slope_long$LC = gsub('LC15', 'Water', slope_long$LC)
slope_long$LC = gsub('LC1', 'Evergreen forest', slope_long$LC)
slope_long$LC = gsub('LC2', 'Deciduous forest', slope_long$LC)
slope_long$LC = gsub('LC3', 'Mixed forest', slope_long$LC)
slope_long$LC = gsub('LC4', 'Woodland', slope_long$LC)
slope_long$LC = gsub('LC5', 'Low shrub', slope_long$LC)
slope_long$LC = gsub('LC6', 'Tall shrub', slope_long$LC)
slope_long$LC = gsub('LC7', 'Open shrubs', slope_long$LC)
slope_long$LC = gsub('LC8', 'Herbaceous', slope_long$LC)
slope_long$LC = gsub('LC9', 'Tussock tundra', slope_long$LC)

# PFT names
slope_long$pft = gsub('allDecShrub', 'Deciduous shrubs', slope_long$pft)
slope_long$pft = gsub('allEvShrub', 'Evergreen shrubs', slope_long$pft)
slope_long$pft = gsub('graminoid', 'Graminoids', slope_long$pft)
slope_long$pft = gsub('tmlichen', 'Lichens', slope_long$pft)
slope_long$pft = gsub('allForb', 'Forbs', slope_long$pft)
slope_long$pft = gsub('alnshr', 'Alder', slope_long$pft)
slope_long$pft = gsub('betshr', 'Birch', slope_long$pft)
slope_long$pft = gsub('salshr', 'Willow', slope_long$pft)

# Caribou density labels 
slope_long$caribou_density = as.factor(slope_long$caribou_density)
slope_long = slope_long %>%
  mutate(caribou_density = fct_recode(caribou_density,
                                      "None" = '1',
                                      "Low" = '2',
                                      "Medium" = '3',
                                      "High" = '4'
  ))

# Reorder PFTs
slope_long$pft = factor(slope_long$pft, levels = c("Deciduous shrubs", "Evergreen shrubs", "Forbs", "Graminoids", "Lichens", "Alder", "Birch", "Willow"))

######################################################################################################
######################################################################################################

# 3. SUMMARIZE DATA ----------------------------------------------------------------------------

data_all = slope_long

# Aggregate over land cover types to summarize by season
data_all_seasonal_summary = data_all %>%
  group_by(pft, season, caribou_density) %>%
  summarise(count = sum(count),
            mean = mean(mean),
            variance = mean(variance))

######################################################################################################
######################################################################################################

# 4. FILTER DATA ----------------------------------------------------------------------------

# Select seasons
data_all_seasonal_summary = data_all_seasonal_summary[(data_all_seasonal_summary$season != 'autumn' & data_all_seasonal_summary$season != 'precalving' & data_all_seasonal_summary$season != 'rut'),]

# Reorder seasons
data_all_seasonal_summary$season = factor(data_all_seasonal_summary$season, levels=c('calving', 'postcalving', 'summer', 'winter', 'overall'))

######################################################################################################
######################################################################################################

# 5. PLOT ----------------------------------------------------------------------------

# 5.1 PFTs -----------------------------------------------------------------

plt_season_pft =
  ggplot(data_all_seasonal_summary[data_all_seasonal_summary$pft %in% pfts,], aes(x = caribou_density, y = mean, group = season))+
  geom_point(size = 4, aes(col = season))+
  geom_line(size = 1, aes(col = season))+
  facet_wrap(~pft)+
  theme_minimal(base_size = 40)+
  scale_fill_manual(values = cb_palette_seasons)+
  scale_colour_manual(values = cb_palette_seasons)+
  theme(legend.position="top",
        legend.title = element_text(size = 36),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 26))+
  labs(y = 'Slope (% per year)', x = "Caribou relative spatial density", col = 'Season')

plt_season_pft

outName = paste0('fig_s14_caribou_density_season_pfts_sens_slopes.jpg')

ggsave(
  paste0(outPath, outName),
  plt_season_pft,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)

# 5.2 Shrubs -----------------------------------------------------------------

plt_season_shrubs =
  ggplot(data_all_seasonal_summary[data_all_seasonal_summary$pft %in% shrubs, ], aes(x = caribou_density, y = mean, group = season))+
  geom_point(size = 4, aes(col = season))+
  geom_line(size = 1, aes(col = season))+
  facet_wrap(~pft)+
  theme_minimal(base_size = 40)+
  scale_fill_manual(values = cb_palette_seasons)+
  scale_colour_manual(values = cb_palette_seasons)+
  theme(legend.position="top",
        legend.title = element_text(size = 36),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 26))+
  labs(y = 'Slope (% per year)', x = "Caribou relative spatial density", col = 'Season')

plt_season_shrubs

outName = paste0('fig_s15_caribou_density_season_shrubs_sens_slopes.jpg')

ggsave(
  paste0(outPath, outName),
  plt_season_shrubs,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)




