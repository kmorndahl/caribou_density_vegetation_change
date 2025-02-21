######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script creates figure showing Fortymile caribou seasonal ranges, with inset map

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Packages ----------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(cowplot)
library(ggspatial)
library(smoothr)
library(units)

# 1.2 Set parameters ----------------------------------------------------------------------------

# Range parameters
iso_level = 0.95 # Isopleth type used for determining seasonal ranges
core_season = 'overall' # Seasons used for determining Core Uplands
core_iso_level = 0.5 # Isopleth type used for determining Core Uplands

# 1.3 Read in data ----------------------------------------------------------------------------

# Read in caribou range data
ranges = sf::read_sf(paste0(inPath, 'range_data/isopleths/1_season/1_population_weighted/iso_seasons_population_weighted.shp')) 
core = sf::read_sf(paste0(inPath, 'range_data/isopleths/1_season/1_population_weighted/iso_seasons_population_weighted_vhf.shp'))
outline = sf::read_sf(paste0(inPath, 'range_data/ssf_outline.shp'))

# Read in map data
base_map = sf::read_sf(paste0(inPath, '/map_data/ne_50m_admin_1_states_provinces.shp'))
roads = sf::read_sf(paste0(inPath, '/map_data/ne_10m_roads.shp'))
rivers = sf::read_sf(paste0(inPath, '/map_data/ne_50m_rivers_lake_centerlines.shp'))
towns = sf::read_sf(paste0(inPath, '/map_data/ne_10m_populated_places.shp'))

# 1.4 Tidy map data ----------------------------------------------------------------------------

# Subset map data
AK_YT_NWT = base_map[(base_map$gn_name == 'Alaska'|base_map$gn_name == 'Yukon'|base_map$gn_name == 'Northwest Territories'),]
AK_YT = base_map[(base_map$gn_name == 'Alaska'|base_map$gn_name == 'Yukon'),]
roads_AK_YT = sf::st_intersection(roads, AK_YT)
rivers_AK_YT = sf::st_intersection(rivers, AK_YT)
towns_AK_YT = sf::st_intersection(towns, AK_YT)

# Reproject map data to match range data
base_map = sf::st_transform(base_map, st_crs(ranges))
AK_YT = sf::st_transform(AK_YT, st_crs(ranges))
roads_AK_YT = sf::st_transform(roads_AK_YT, st_crs(ranges))
rivers_AK_YT = sf::st_transform(rivers_AK_YT, st_crs(ranges))
towns_AK_YT = sf::st_transform(towns_AK_YT, st_crs(ranges))
outline = sf::st_transform(outline, st_crs(ranges))

# Filter map data
towns_AK_YT_filtered = towns_AK_YT[(towns_AK_YT$NAME == 'Circle' | towns_AK_YT$NAME == 'Dawson City' | towns_AK_YT$NAME == 'Eagle' | towns_AK_YT$NAME == 'Fairbanks'),]
towns_AK_YT_dawson = towns_AK_YT[(towns_AK_YT$NAME == 'Circle' | towns_AK_YT$NAME == 'Dawson City' | towns_AK_YT$NAME == 'Eagle' | towns_AK_YT$NAME == 'Fairbanks'),]
rivers_AK_YT_filtered = rivers_AK_YT[(rivers_AK_YT$name_en == 'Tanana' | rivers_AK_YT$name_en == 'Yukon'),]

# Get extent
ext = sf::st_bbox(st_union(towns_AK_YT_filtered, outline))

# Set up rivers for labeling
rivers_AK_YT_labels = sf::st_crop(rivers_AK_YT_filtered, ext)
rivers_AK_YT_labels = rivers_AK_YT_labels[rivers_AK_YT_labels$type_en == 'State',]
yukon_river = rivers_AK_YT_labels[rivers_AK_YT_labels$name == 'Yukon',]
tanana_river = rivers_AK_YT_labels[rivers_AK_YT_labels$name == 'Tanana',]
yukon_river$name_en = gsub('Yukon', 'Yukon River', yukon_river$name_en)
tanana_river$name_en = gsub('Tanana', 'Tanana River', tanana_river$name_en)

# Set up roads for labeling
roads_AK_YT_labels = sf::st_crop(roads_AK_YT, ext)
alaska_highway = roads_AK_YT_labels[(roads_AK_YT_labels$name == '2' & roads_AK_YT_labels$length_km == 318),]
steese_highway = roads_AK_YT_labels[roads_AK_YT_labels$name == '6',]
taylor_highway = roads_AK_YT_labels[(roads_AK_YT_labels$name == '5' & roads_AK_YT_labels$iso_a2 == 'US'),]
alaska_highway$name = gsub('2', 'Alaska Highway', alaska_highway$name)
taylor_highway$name = gsub('5', 'Taylor Highway', taylor_highway$name)
steese_highway$name = gsub('6', 'Steese Highway', steese_highway$name)

# Set up towns for labeling
towns_AK_YT_labels_main = towns_AK_YT[(towns_AK_YT$NAME == 'Circle' | towns_AK_YT$NAME == 'Eagle' | towns_AK_YT$NAME == 'Fairbanks'),]
fairbanks = towns_AK_YT[towns_AK_YT$NAME == 'Fairbanks',]
circle = towns_AK_YT[towns_AK_YT$NAME == 'Circle',]
eagle = towns_AK_YT[towns_AK_YT$NAME == 'Eagle',]
dawson = towns_AK_YT[towns_AK_YT$NAME == 'Dawson City',]

# 1.5 Tidy range data ----------------------------------------------------------------------------

# Subset range data
core = core[(core$season == core_season & core$isopleth == core_iso_level),]
ranges = ranges[ranges$isopleth == iso_level,]

# Reorder seasons
ranges = ranges[ranges$season != 'overall',]
ranges$season = factor(ranges$season, levels=c('winter', 'rut', 'autumn', 'precalving', 'summer', 'postcalving', 'calving'))

# Smooth seasonal polygons
ranges_smooth = smoothr::drop_crumbs(ranges, units::set_units(100000000, m^2))
ranges_smooth = smoothr::smooth(ranges_smooth, method = "ksmooth", smoothness = 2)

# Smooth Core Uplands
core_smooth = smoothr::drop_crumbs(core, units::set_units(100000000, m^2))
core_smooth = smoothr::smooth(core_smooth, method = "ksmooth", smoothness = 2)

######################################################################################################
######################################################################################################

# 2. PLOT ----------------------------------------------------------------------------

# 2.1 Main map ----------------------------------------------------------------------------

main = ggplot(data = ranges) + 
  
  # Background and ranges
  geom_sf(data = AK_YT_NWT, fill = 'white')+
  geom_sf(data = ranges_smooth, aes(fill = season, col = season), linewidth = 1, alpha = 0.1)+
  
  # Rivers, roads and towns
  geom_sf(data = rivers_AK_YT_filtered, col = 'blue2', linewidth = 0.75, alpha = 0.5)+
  geom_sf(data = roads_AK_YT[!((roads_AK_YT$scalerank >= 8) & (roads_AK_YT$sov_a3 == 'CAN')),], linewidth = 0.75, col = 'black', alpha = 0.5)+
  geom_sf(data = towns_AK_YT_filtered, size = 4, col = 'black')+
  
  # Refine rivers and towns
  geom_sf_text(data = yukon_river, aes(label = name_en), hjust = -0, vjust = -0.5, col = 'blue2', size = 10)+
  geom_sf_text(data = tanana_river, aes(label = name_en), hjust = 1, vjust = 1, col = 'blue2', size = 10)+
  geom_sf_text(data = fairbanks, aes(label = NAME, hjust = 0.9, vjust = 1.75), size = 10, col = 'black')+
  geom_sf_text(data = circle, aes(label = NAME, hjust = 0, vjust = -0.5), size = 10, col = 'black')+
  geom_sf_text(data = eagle, aes(label = NAME, hjust = 0, vjust = -0.5), size = 10, col = 'black')+
  geom_sf_text(data = dawson, aes(label = NAME, hjust = 0.7, vjust = 1.5), size = 10, col = 'black')+
  
  # Core and extended range
  geom_sf(data = core_smooth, lty = 'dashed', col = 'black', linewidth = 1.75, fill = NA)+
  geom_sf(data = outline, lty = 'dashed', col = 'black', linewidth = 1.75, fill = NA)+
  
  # Plot parameters
  theme_grey(base_size = 34)+
  labs(fill = 'Season', color = 'Season', x = '', y = '')+
  viridis::scale_color_viridis(discrete = TRUE, option = "C", direction = -1)+ # https://sjmgarnier.github.io/viridis/reference/scale_viridis.html
  viridis::scale_fill_viridis(discrete = TRUE, option = "C", direction = -1)+ # https://sjmgarnier.github.io/viridis/reference/scale_viridis.html
  coord_sf(xlim = c(ext$xmin - 10000, ext$xmax + 150000), ylim = c(ext$ymin, ext$ymax))+
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 2, height = unit(0.5, "cm"))+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.position = c(0.925, 0.3),
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.background = element_rect(fill="transparent")) 

main

# 2.2 Inset map ----------------------------------------------------------------------------

# Get extent for inset
ext_inset = st_bbox(AK_YT)

# Get background for inset
inset_map = base_map[(base_map$iso_a2 == 'US' | base_map$iso_a2 == 'CA'),]

# Plot
inset = ggplot() + 
  geom_sf(data = inset_map, fill = "white", linewidth = 0.75)+
  geom_sf(data = st_as_sfc(st_bbox(ext)), fill = NA, col = 'red', linewidth = 1)+
  coord_sf(xlim = c(ext_inset$xmin, ext_inset$xmax), ylim = c(ext_inset$ymin, ext_inset$ymax))+
  theme_void()
inset

# 2.3 Join maps ----------------------------------------------------------------------------

final = ggdraw() +
  draw_plot(main) +
  draw_plot(inset, x = 0.6, y = 0.6, width = 0.4, height = 0.4)
final

######################################################################################################
######################################################################################################

# 3. SAVE ----------------------------------------------------------------------------

# 3.1 Final map ----------------------------------------------------------------------------

outName = paste0('fig_1_seasonal_range_map_iso', iso_level*100, '.jpg')

ggsave(
  paste0(outPath, outName),
  final,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)

# 3.2 Inset map ----------------------------------------------------------------------------

ggsave(
  paste0(outPath, 'fig_1_inset.png'),
  inset,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'transparent',
  dpi = 600
)
