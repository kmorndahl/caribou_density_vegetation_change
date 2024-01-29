######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots time series of PFT change over the study period

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/veg_data/weighted_area/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Parameters ----------------------------------------------------------------------------

# Set season for summaries
selected_season = 'overall'

# Set number visualization
options(scipen = 999)

# Set palette
cb_palette_long = c("#88CCEE", "#117733", "#DDCC77","#CC6677", "#332288", "#AA4499", 
                    "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

# 1.2 Packages ----------------------------------------------------------------------------

library(tidyverse)

# 1.3 Read in data ----------------------------------------------------------------------------

autumn = read.csv(paste0(inPath, 'weighted_area_annual_autumn.csv'))
calving = read.csv(paste0(inPath, 'weighted_area_annual_calving.csv'))
postcalving = read.csv(paste0(inPath, 'weighted_area_annual_postcalving.csv'))
precalving = read.csv(paste0(inPath, 'weighted_area_annual_precalving.csv'))
rut = read.csv(paste0(inPath, 'weighted_area_annual_rut.csv'))
summer = read.csv(paste0(inPath, 'weighted_area_annual_summer.csv'))
winter = read.csv(paste0(inPath, 'weighted_area_annual_winter.csv'))
overall = read.csv(paste0(inPath, 'weighted_area_annual_overall.csv'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# 2.1 Combine data -----------------------------------------------------------------

cover_density = rbind(autumn, calving, postcalving, precalving, rut, summer, winter, overall)
cover_density = dplyr::select(cover_density, -c(system.index, .geo))
cover_density = dplyr::select(cover_density, -contains("_count"))

# 2.2 Pivot longer -----------------------------------------------------------------

cover_density_long = cover_density %>%
  pivot_longer(!c(LC, caribou_density, season, area_sum), names_to = 'pft', values_to = 'weighted_area') %>%
  separate(pft, c("pft", "year", NA, NA)) %>%
  rename(total_area = area_sum)
cover_density_long$year = as.numeric(gsub('X', '', cover_density_long$year))

# 2.3 Tidy data -----------------------------------------------------------------

# Filter out zero area
cover_density_long = cover_density_long[cover_density_long$total_area > 0,]

# Land cover names
cover_density_long$LC = gsub('LC10', 'Sparsely vegetated', cover_density_long$LC)
cover_density_long$LC = gsub('LC11', 'Fen', cover_density_long$LC)
cover_density_long$LC = gsub('LC12', 'Bog', cover_density_long$LC)
cover_density_long$LC = gsub('LC13', 'Shallows/littoral', cover_density_long$LC)
cover_density_long$LC = gsub('LC14', 'Barren', cover_density_long$LC)
cover_density_long$LC = gsub('LC15', 'Water', cover_density_long$LC)
cover_density_long$LC = gsub('LC1', 'Evergreen forest', cover_density_long$LC)
cover_density_long$LC = gsub('LC2', 'Deciduous forest', cover_density_long$LC)
cover_density_long$LC = gsub('LC3', 'Mixed forest', cover_density_long$LC)
cover_density_long$LC = gsub('LC4', 'Woodland', cover_density_long$LC)
cover_density_long$LC = gsub('LC5', 'Low shrub', cover_density_long$LC)
cover_density_long$LC = gsub('LC6', 'Tall shrub', cover_density_long$LC)
cover_density_long$LC = gsub('LC7', 'Open shrubs', cover_density_long$LC)
cover_density_long$LC = gsub('LC8', 'Herbaceous', cover_density_long$LC)
cover_density_long$LC = gsub('LC9', 'Tussock tundra', cover_density_long$LC)

# PFT names
cover_density_long$pft = gsub('allDecShrub', 'Deciduous shrubs', cover_density_long$pft)
cover_density_long$pft = gsub('allEvShrub', 'Evergreen shrubs', cover_density_long$pft)
cover_density_long$pft = gsub('graminoid', 'Graminoids', cover_density_long$pft)
cover_density_long$pft = gsub('tmlichen', 'Lichens', cover_density_long$pft)
cover_density_long$pft = gsub('allForb', 'Forbs', cover_density_long$pft)
cover_density_long$pft = gsub('alnshr', 'Alder', cover_density_long$pft)
cover_density_long$pft = gsub('betshr', 'Birch', cover_density_long$pft)
cover_density_long$pft = gsub('salshr', 'Willow', cover_density_long$pft)

# Caribou density labels 
cover_density_long$caribou_density = as.factor(cover_density_long$caribou_density)
cover_density_long = cover_density_long %>%
  mutate(caribou_density = fct_recode(caribou_density,
                                      "None" = '1',
                                      "Low" = '2',
                                      "Medium" = '3',
                                      "High" = '4'))

# Reorder PFTs
cover_density_long$pft = factor(cover_density_long$pft, levels = c("Deciduous shrubs", "Evergreen shrubs", "Forbs", "Graminoids", "Lichens", "Alder", "Birch", "Willow"))

# Subset
cover_density_long = cover_density_long[cover_density_long$year >= 1992,]

######################################################################################################
######################################################################################################

# 3. SUMMARIZE DATA ----------------------------------------------------------------------------

# 3.1 Summarize -----------------------------------------------------------------

data_all = cover_density_long

# Aggregate over land cover types to summarize by season
data_all_seasonal_summary = data_all %>%
  group_by(pft, season, caribou_density, year) %>%
  summarise(total_area = sum(total_area),
            weighted_area = sum(weighted_area))

# Aggregate using overall season to summarize by land cover type
data_all_lc_summary = data_all[data_all$season == selected_season,] %>%
  group_by(pft, LC, caribou_density, year) %>%
  summarise(total_area = sum(total_area),
            weighted_area = sum(weighted_area))

# Aggregate over land cover types for the overall season to get overall summaries
data_all_overall_summary = data_all[data_all$season == selected_season,] %>%
  group_by(pft, caribou_density, year) %>%
  summarise(total_area = sum(total_area),
            weighted_area = sum(weighted_area))

# 3.2 Calculate percentages -----------------------------------------------------------------

data_all$percent_cover = (data_all$weighted_area/data_all$total_area)*100
data_all_overall_summary$percent_cover = (data_all_overall_summary$weighted_area/data_all_overall_summary$total_area)*100
data_all_seasonal_summary$percent_cover = (data_all_seasonal_summary$weighted_area/data_all_seasonal_summary$total_area)*100
data_all_lc_summary$percent_cover = (data_all_lc_summary$weighted_area/data_all_lc_summary$total_area)*100

######################################################################################################
######################################################################################################

# 4. PLOT ----------------------------------------------------------------------------

plt =
  ggplot(data_all_overall_summary, aes(x = year, y = percent_cover, group = caribou_density))+
  geom_point(size = 3, aes(col = caribou_density))+
  geom_line(size = 2, aes(col = caribou_density))+
  facet_wrap(~pft, scales = "free")+
  theme_minimal(base_size = 40)+
  scale_fill_manual(values = cb_palette_long)+
  scale_colour_manual(values = cb_palette_long)+
  scale_x_continuous(breaks=seq(1990,2020,10))+
  xlim(1990, 2020)+
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.title = element_text(size = 36),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 26))+
  labs(y = 'Top cover (%)', x = "Year", col = 'Caribou relative\nspatial density')

plt

######################################################################################################
######################################################################################################

# 5. SAVE ----------------------------------------------------------------------------

outName = paste0('fig_s7_caribou_density_weighted_area_time_series.jpg')

ggsave(
  paste0(outPath, outName),
  plt,
  width = 40,
  height = 40,
  units = 'cm',
  bg = 'white',
  dpi = 600
)

