################################################################################
################################################################################

# CODE DESCRIPTION

# This script plots PFT change partitioned by caribou density, season, and land cover

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

################################################################################
################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/veg_data/weighted_area/'

outPath = 'figures/'

################################################################################
################################################################################

# 1. SET UP --------------------------------------------------------------------

# 1.1 Parameters ---------------------------------------------------------------

# Set season for summaries
selected_season = 'overall'

# Set start and end year
start_year = 1992
end_year = 2020

# Set list of PFTs
pfts = c('Deciduous shrubs', 'Evergreen shrubs', 'Forbs', 'Graminoids', 'Lichens')

# Set list of shrubs
shrubs = c('Alder', 'Birch', 'Willow')

# Set number visualization
options(scipen = 999)

# Set palettes
cb_palette_pft = c("#117733", "#CC6677", "#88CCEE", "#332288", "#DDCC77", "#AA4499", 
                   "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
cb_palette_seasons = c("#DDCC77", "#CC6677", "#117733", "#88CCEE", "#332288", "#AA4499", 
                       "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
cb_palette_long = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                    "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

# 1.2 Packages -----------------------------------------------------------------

library(tidyverse)
library(stringr)

# 1.3 Read in data -------------------------------------------------------------

autumn = read.csv(paste0(inPath, 'weighted_area_annual_autumn.csv'))
calving = read.csv(paste0(inPath, 'weighted_area_annual_calving.csv'))
postcalving = read.csv(paste0(inPath, 'weighted_area_annual_postcalving.csv'))
precalving = read.csv(paste0(inPath, 'weighted_area_annual_precalving.csv'))
rut = read.csv(paste0(inPath, 'weighted_area_annual_rut.csv'))
summer = read.csv(paste0(inPath, 'weighted_area_annual_summer.csv'))
winter = read.csv(paste0(inPath, 'weighted_area_annual_winter.csv'))
overall = read.csv(paste0(inPath, 'weighted_area_annual_overall.csv'))

################################################################################
################################################################################

# 2. TIDY ----------------------------------------------------------------------

# 2.1 Combine data -------------------------------------------------------------

cover_density = rbind(autumn, calving, postcalving, precalving, rut, summer, winter, overall)
cover_density = dplyr::select(cover_density, -any_of(c('system.index', '.geo')))
cover_density = dplyr::select(cover_density, -contains("_count"))

# 2.2 Format data --------------------------------------------------------------

# Pivot longer
cover_density_long = cover_density %>%
  pivot_longer(!c(LC, caribou_density, season, area_sum), names_to = 'pft', values_to = 'weighted_area') %>%
  separate(pft, c("pft", "year", NA, NA)) %>%
  rename(total_area = area_sum)
cover_density_long$year = as.numeric(gsub('X', '', cover_density_long$year))

# Subset to start and end year
cover_density_long = cover_density_long[cover_density_long$year == start_year | cover_density_long$year == end_year,]
cover_density_long$year = gsub(start_year, 'weighted_area_start', cover_density_long$year)
cover_density_long$year = gsub(end_year, 'weighted_area_end', cover_density_long$year)

# 2.3 Tidy data ----------------------------------------------------------------

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

# 2.4 Save tidy data -----------------------------------------------------------

write.csv(cover_density_long, paste0(inPath, 'pft_area_caribou_density_seasonal.csv'), row.names = FALSE)

################################################################################
################################################################################

# 3. SUMMARIZE DATA ------------------------------------------------------------

# 3.1 Summarize ----------------------------------------------------------------

data_all = cover_density_long

# Aggregate over land cover types to summarize by season
data_all_seasonal_summary = data_all %>%
  group_by(pft, season, caribou_density, year) %>%
  summarise(total_area = sum(total_area),
            weighted_area = sum(weighted_area)) %>%
  pivot_wider(names_from = year, values_from = weighted_area)

# Aggregate using overall season to summarize by land cover type
data_all_lc_summary = data_all[data_all$season == selected_season,] %>%
  group_by(pft, LC, caribou_density, year) %>%
  summarise(total_area = sum(total_area),
            weighted_area = sum(weighted_area)) %>%
  pivot_wider(names_from = year, values_from = weighted_area)

# Aggregate over land cover types for the overall season to get overall summaries
data_all_overall_summary = data_all[data_all$season == selected_season,] %>%
  group_by(pft, caribou_density, year) %>%
  summarise(total_area = sum(total_area),
            weighted_area = sum(weighted_area)) %>%
  pivot_wider(names_from = year, values_from = weighted_area)

# 3.2 Calculate percentages ----------------------------------------------------

# Start year percent cover
data_all_overall_summary$percent_cover_start = (data_all_overall_summary$weighted_area_start/data_all_overall_summary$total_area)*100
data_all_seasonal_summary$percent_cover_start = (data_all_seasonal_summary$weighted_area_start/data_all_seasonal_summary$total_area)*100
data_all_lc_summary$percent_cover_start = (data_all_lc_summary$weighted_area_start/data_all_lc_summary$total_area)*100

# End year percent cover
data_all_overall_summary$percent_cover_end = (data_all_overall_summary$weighted_area_end/data_all_overall_summary$total_area)*100
data_all_seasonal_summary$percent_cover_end = (data_all_seasonal_summary$weighted_area_end/data_all_seasonal_summary$total_area)*100
data_all_lc_summary$percent_cover_end = (data_all_lc_summary$weighted_area_end/data_all_lc_summary$total_area)*100

# Change - absolute percent cover
data_all_overall_summary$absolute_percent_difference = data_all_overall_summary$percent_cover_end-data_all_overall_summary$percent_cover_start
data_all_seasonal_summary$absolute_percent_difference = data_all_seasonal_summary$percent_cover_end-data_all_seasonal_summary$percent_cover_start
data_all_lc_summary$absolute_percent_difference = data_all_lc_summary$percent_cover_end-data_all_lc_summary$percent_cover_start

# Change - relative percent cover
data_all_overall_summary$relative_percent_difference = (data_all_overall_summary$absolute_percent_difference/data_all_overall_summary$percent_cover_start)*100
data_all_seasonal_summary$relative_percent_difference = (data_all_seasonal_summary$absolute_percent_difference/data_all_seasonal_summary$percent_cover_start)*100
data_all_lc_summary$relative_percent_difference = (data_all_lc_summary$absolute_percent_difference/data_all_lc_summary$percent_cover_start)*100

################################################################################
################################################################################

# 4. FILTER DATA ---------------------------------------------------------------

# Remove unnecessary land cover types
data_all_lc_summary = data_all_lc_summary[(data_all_lc_summary$LC != 'Evergreen forest' & data_all_lc_summary$LC != 'Deciduous forest' & data_all_lc_summary$LC != 'Mixed forest' & data_all_lc_summary$LC != 'Fen' & data_all_lc_summary$LC != 'Bog' & data_all_lc_summary$LC != 'Shallows/littoral' & data_all_lc_summary$LC != 'Water'),]

# Select seasons
data_all_seasonal_summary = data_all_seasonal_summary[(data_all_seasonal_summary$season != 'autumn' & data_all_seasonal_summary$season != 'precalving' & data_all_seasonal_summary$season != 'rut'),]

# Reorder seasons
data_all_seasonal_summary$season = factor(data_all_seasonal_summary$season, levels=c('calving', 'postcalving', 'summer', 'winter', 'overall'))

# Remove evergreen shrubs and forbs from season and land cover summaries
data_all_seasonal_summary = data_all_seasonal_summary[(data_all_seasonal_summary$pft != 'Evergreen shrubs' & data_all_seasonal_summary$pft != 'Forbs'),]
data_all_lc_summary = data_all_lc_summary[(data_all_lc_summary$pft != 'Evergreen shrubs' & data_all_lc_summary$pft != 'Forbs'),]

################################################################################
################################################################################

# 5. PLOT ----------------------------------------------------------------------

# Set up grouping parameters
ds_datasets = list(data_all_seasonal_summary, data_all_lc_summary)
ds_palettes = list(cb_palette_seasons, cb_palette_long)
ds_x_vars = c('season', 'LC')
ds_legend = c('Season', "Land\ncover\ntype")
ds_vjust = c(0, 5)
veg_names = c('pfts', 'shrubs')
veg_lookup = list(pfts = pfts, shrubs = shrubs) 
response_types = c('absolute_percent_difference', 'relative_percent_difference')

fig_name_lookup = data.frame(x_var = rep(ds_x_vars, each = 4), 
                             veg_name = rep(rep(veg_names, each = 2), 2), 
                             response_type = rep(response_types, 4),
                             fig_name = c('fig_5_', 'fig_s16_', 'fig_6_', 'fig_s19_', 'fig_s17_', 'fig_s18_', 'fig_s20_', 'fig_s21_'))

for(i in 1:length(ds_datasets)){
  
  # Get dataset
  dataset = ds_datasets[[i]]
  
  # Get palette
  palette = ds_palettes[[i]]
  
  # Get X variable
  x_var = ds_x_vars[[i]]
  
  # Get legend title
  legend = ds_legend[[i]]
  
  # Get vjust
  vjust = ds_vjust[[i]]
  
  for(veg_name in veg_names){
    
    # Subset data
    plt_data = dataset[dataset$pft %in% veg_lookup[veg_name][[1]],]
    
    for(response_type in response_types){
      
      # Get y axis label information
      y_lab_type = str_to_title(strsplit(response_type, '_')[[1]][1])
      
      plt =
        ggplot(plt_data, aes(x = caribou_density, y = !!sym(response_type), group = !!sym(x_var)))+
        geom_point(size = 4, aes(col = !!sym(x_var)))+
        geom_line(size = 1, aes(col = !!sym(x_var)))+
        facet_wrap(~pft)+
        theme_minimal(base_size = 40)+
        scale_fill_manual(values = palette)+
        scale_colour_manual(values = palette)+
        theme(legend.position = "top",
              legend.title = element_text(size = 32, vjust = vjust),
              legend.title.align = 0.5,
              legend.text = element_text(size = 26),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 26))+
        labs(y = paste0(y_lab_type, ' change \nin top cover (%)'), x = "Caribou relative spatial density", col = legend)
      plt
      
      fig_name = fig_name_lookup$fig_name[fig_name_lookup$x_var == x_var & fig_name_lookup$veg_name == veg_name & fig_name_lookup$response_type == response_type]
      
      outName = paste0(fig_name, 'caribou_density_', x_var, '_', veg_name, '_', response_type, '.jpg')
      
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
    
  }
  
}