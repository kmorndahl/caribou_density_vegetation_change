######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots categorical PFT change partitioned by caribou density

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/veg_data/categorical_change/'

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

# Set number visualization
options(scipen = 999)

# Set palettes
change_palette = c("#332288", "#6699CC", "#888888", "#CC6677", "#882255")

# 1.2 Packages ----------------------------------------------------------------------------

library(tidyverse)
library(stringr)

# 1.3 Read in data ----------------------------------------------------------------------------

autumn = read.csv(paste0(inPath, 'categorical_change_autumn.csv'))
calving = read.csv(paste0(inPath, 'categorical_change_calving.csv'))
postcalving = read.csv(paste0(inPath, 'categorical_change_postcalving.csv'))
precalving = read.csv(paste0(inPath, 'categorical_change_precalving.csv'))
rut = read.csv(paste0(inPath, 'categorical_change_rut.csv'))
summer = read.csv(paste0(inPath, 'categorical_change_summer.csv'))
winter = read.csv(paste0(inPath, 'categorical_change_winter.csv'))
overall = read.csv(paste0(inPath, 'categorical_change_overall.csv'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# 2.1 Combine data -----------------------------------------------------------------

# Evergreen shrub has no significant decline, add dummy column to facilitate data combination
overall$allEvShrub_count_neg2 = 0

categorical_change = rbind(autumn, calving, postcalving, precalving, rut, summer, winter, overall)
categorical_change = dplyr::select(categorical_change, -c(system.index, .geo))

# 2.2 Format data -----------------------------------------------------------------

# Pivot longer
categorical_change_long = pivot_longer(categorical_change, !c(LC, caribou_density, season), names_to = 'pft', values_to = 'change_count')
categorical_change_long = separate(categorical_change_long, pft, c("pft", NA, "change_type"))
categorical_change_long$change_count = as.numeric(categorical_change_long$change_count)

# 2.3 Tidy data -----------------------------------------------------------------

# Change type labels
categorical_change_long$change_type = gsub('neg2', '|Decrease| >= RMSE', categorical_change_long$change_type)
categorical_change_long$change_type = gsub('neg1', 'Decrease', categorical_change_long$change_type)
categorical_change_long$change_type = gsub('0', 'No change', categorical_change_long$change_type)
categorical_change_long$change_type = gsub('1', 'Increase', categorical_change_long$change_type)
categorical_change_long$change_type = gsub('2', 'Increase >= RMSE', categorical_change_long$change_type)

# Reorder change types
categorical_change_long$change_type = factor(categorical_change_long$change_type, levels = c("Increase >= RMSE", "Increase", "No change", "Decrease", "|Decrease| >= RMSE"))

# Land cover names
categorical_change_long$LC = gsub('LC10', 'Sparsely vegetated', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC11', 'Fen', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC12', 'Bog', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC13', 'Shallows/littoral', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC14', 'Barren', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC15', 'Water', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC1', 'Evergreen forest', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC2', 'Deciduous forest', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC3', 'Mixed forest', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC4', 'Woodland', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC5', 'Low shrub', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC6', 'Tall shrub', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC7', 'Open shrubs', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC8', 'Herbaceous', categorical_change_long$LC)
categorical_change_long$LC = gsub('LC9', 'Tussock tundra', categorical_change_long$LC)

# PFT names
categorical_change_long$pft = gsub('allDecShrub', 'Deciduous shrubs', categorical_change_long$pft)
categorical_change_long$pft = gsub('allEvShrub', 'Evergreen shrubs', categorical_change_long$pft)
categorical_change_long$pft = gsub('graminoid', 'Graminoids', categorical_change_long$pft)
categorical_change_long$pft = gsub('tmlichen', 'Lichens', categorical_change_long$pft)
categorical_change_long$pft = gsub('allForb', 'Forbs', categorical_change_long$pft)
categorical_change_long$pft = gsub('alnshr', 'Alder', categorical_change_long$pft)
categorical_change_long$pft = gsub('betshr', 'Birch', categorical_change_long$pft)
categorical_change_long$pft = gsub('salshr', 'Willow', categorical_change_long$pft)

# Reorder PFTs
categorical_change_long$pft = factor(categorical_change_long$pft, levels = c("Deciduous shrubs", "Evergreen shrubs", "Forbs", "Graminoids", "Lichens", "Alder", "Birch", "Willow"))

# Caribou density labels 
categorical_change_long$caribou_density = as.factor(categorical_change_long$caribou_density)
categorical_change_long = categorical_change_long %>%
  mutate(caribou_density = fct_recode(caribou_density,
                                      "None" = '1',
                                      "Low" = '2',
                                      "Medium" = '3',
                                      "High" = '4'
  ))

# 2.4 Save tidy data -----------------------------------------------------------

write.csv(categorical_change_long, paste0(inPath, 'pft_categorical_change_caribou_density_seasonal.csv'), row.names = FALSE)

######################################################################################################
######################################################################################################

# 3. SUMMARIZE DATA ----------------------------------------------------------------------------

# NA means there were no pixels within a given LC/caribou density grouping, remove these
data_all = na.omit(categorical_change_long)

# Aggregate over land cover types for the overall season to get overall summaries
data_all_overall_summary = data_all[data_all$season == selected_season,] %>%
  group_by(pft, caribou_density, change_type) %>%
  summarise(change_count = sum(change_count))

######################################################################################################
######################################################################################################

# 4. PLOT ----------------------------------------------------------------------------

# 4.1 PFTs -----------------------------------------------------------------

plt_categorical_pft = ggplot(data_all_overall_summary[data_all_overall_summary$pft %in% pfts,], aes(fill = change_type, y = change_count, x=caribou_density)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~pft)+
  theme_minimal(base_size = 40)+
  scale_fill_manual(values = change_palette)+
  scale_colour_manual(values = change_palette)+
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(30, "points"),
        legend.text=element_text(size = 26),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))+
  guides(fill=guide_legend(nrow = 2, byrow = TRUE))+
  labs(y = 'Proportion of pixels', x = "Caribou relative spatial density", fill = '')

plt_categorical_pft

outName = paste0('fig_s14_caribou_density_pfts_categorical_change.jpg')

ggsave(
  paste0(outPath, outName),
  plt_categorical_pft,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)

# 4.2 Shrubs -----------------------------------------------------------------

plt_categorical_shrub = ggplot(data_all_overall_summary[data_all_overall_summary$pft %in% shrubs,], aes(fill = change_type, y = change_count, x=caribou_density)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~pft)+
  theme_minimal(base_size = 40)+
  scale_fill_manual(values = change_palette)+
  scale_colour_manual(values = change_palette)+
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(30, "points"),
        legend.text=element_text(size = 26),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))+
  guides(fill=guide_legend(nrow = 2, byrow = TRUE))+
  labs(y = 'Proportion of pixels', x = "Caribou relative spatial density", fill = '')

plt_categorical_shrub

outName = paste0('fig_s15_caribou_density_shrubs_categorical_change.jpg')

ggsave(
  paste0(outPath, outName),
  plt_categorical_shrub,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)
