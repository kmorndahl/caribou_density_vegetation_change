######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script creates figure showing Fortymile caribou annual, seasonal range centroids

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

# NOTE:

# Caribou season-year isopleths are sensitive and cannot be shared publicly
# To request access to season-year isopleths, contact:

# Torsten Bentzen, Wildlife Biologist
# Division of Wildlife Conservation
# Alaska Department of Fish and Game
# torsten.bentzen@alaska.gov

# And request the following data:

# Season-year isopleths for Fortymile caribou fitting the following criteria:
#   - Years: 1991-2021
#   - Seasons: all seasons
#   - Isopleth levels: 0.95, 0.5 

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Set parameters ----------------------------------------------------------------------------

# Set season for calculating centroids
season = 'calving'

# Set isopleth type for calculating centroids
iso_level = 0.5 # Choose 0.95 or 0.5

# Set season for determining Core Uplands
core_season = 'overall' 

# Set isopleth for determining Core Uplands
core_iso_level = 0.5

# 1.2 Packages ----------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(ggpmisc)
library(cowplot)
library(ggspatial)

# 1.3 Read in data ----------------------------------------------------------------------------

# Read in caribou range data
ranges = sf::read_sf(paste0(inPath, 'range_data/isopleths/0_season_year/0_orig/iso_all_overlap.shp')) 
core = sf::read_sf(paste0(inPath, 'range_data/isopleths/1_season/1_population_weighted/iso_seasons_population_weighted_vhf.shp'))

# Read in map data
base_map = sf::read_sf(paste0(inPath, '/map_data/ne_50m_admin_1_states_provinces.shp'))
roads = sf::read_sf(paste0(inPath, '/map_data/ne_10m_roads.shp'))
rivers = sf::read_sf(paste0(inPath, '/map_data/ne_50m_rivers_lake_centerlines.shp'))
towns = sf::read_sf(paste0(inPath, '/map_data/ne_10m_populated_places.shp'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# 2.1 Tidy range data ----------------------------------------------------------------------------

# Subset Core Uplands
core = core[(core$season == core_season & core$isopleth == core_iso_level),]

# Smooth Core Uplands
core = smoothr::drop_crumbs(core, units::set_units(100000000, m^2))
core = smoothr::smooth(core, method = "ksmooth", smoothness = 2)

# Tidy season-year ranges
ranges$year = as.numeric(ranges$year)

# Cast to polygons instead of multipolygons
ranges_poly = st_cast(ranges, "POLYGON")

# Get centroids
ranges_centroids = sf::st_centroid(ranges_poly)

# Filter isopleths and centroids
ranges_poly_subset = ranges[(ranges$season == season & ranges$level == iso_level),]
ranges_centroid_subset = ranges_centroids[(ranges_centroids$season == season & ranges_centroids$level == iso_level),]

# 2.2 Tidy map data ----------------------------------------------------------------------------

# Subset map data
AK_YT = base_map[(base_map$gn_name == 'Alaska'|base_map$gn_name == 'Yukon'),]
roads_AK_YT = st_intersection(roads, AK_YT)
rivers_AK_YT = st_intersection(rivers, AK_YT)
towns_AK_YT = st_intersection(towns, AK_YT)

# Reproject map data to match isopleths
base_map = st_transform(base_map, st_crs(ranges))
AK_YT = st_transform(AK_YT, st_crs(ranges))
roads_AK_YT = st_transform(roads_AK_YT, st_crs(ranges))
rivers_AK_YT = st_transform(rivers_AK_YT, st_crs(ranges))
towns_AK_YT = st_transform(towns_AK_YT, st_crs(ranges))

# Filter map data
towns_AK_YT_filtered = towns_AK_YT[(towns_AK_YT$NAME == 'Circle' | towns_AK_YT$NAME == 'Dawson City' | towns_AK_YT$NAME == 'Eagle' | towns_AK_YT$NAME == 'Fairbanks'),]
towns_AK_YT_dawson = towns_AK_YT[(towns_AK_YT$NAME == 'Circle' | towns_AK_YT$NAME == 'Dawson City' | towns_AK_YT$NAME == 'Eagle' | towns_AK_YT$NAME == 'Fairbanks'),]
rivers_AK_YT_filtered = rivers_AK_YT[(rivers_AK_YT$name_en == 'Tanana' | rivers_AK_YT$name_en == 'Yukon'),]

# Get extent
ext = st_bbox(st_union(towns_AK_YT_filtered, ranges_poly_subset))

# Set up rivers for labeling
rivers_AK_YT_labels = st_crop(rivers_AK_YT_filtered, ext)
rivers_AK_YT_labels = rivers_AK_YT_labels[rivers_AK_YT_labels$type_en == 'State',]
rivers_AK_YT_labels$name_en = gsub('Yukon', 'Yukon River', rivers_AK_YT_labels$name_en)
rivers_AK_YT_labels$name_en = gsub('Tanana', 'Tanana River', rivers_AK_YT_labels$name_en)

# Set up roads for labeling
roads_AK_YT_labels = st_crop(roads_AK_YT, ext)
alaska_highway = roads_AK_YT_labels[(roads_AK_YT_labels$name == '2' & roads_AK_YT_labels$length_km == 318),]
steese_highway = roads_AK_YT_labels[roads_AK_YT_labels$name == '6',]
taylor_highway = roads_AK_YT_labels[(roads_AK_YT_labels$name == '5' & roads_AK_YT_labels$iso_a2 == 'US'),]
roads_AK_YT_labels = rbind(alaska_highway, steese_highway, taylor_highway)
roads_AK_YT_labels$name = gsub('2', 'Alaska Highway', roads_AK_YT_labels$name)
roads_AK_YT_labels$name = gsub('5', 'Taylor Highway', roads_AK_YT_labels$name)
roads_AK_YT_labels$name = gsub('6', 'Steese Highway', roads_AK_YT_labels$name)

# Set up towns for labeling
towns_AK_YT_labels_main = towns_AK_YT[(towns_AK_YT$NAME == 'Circle' | towns_AK_YT$NAME == 'Eagle' | towns_AK_YT$NAME == 'Fairbanks'),]
towns_AK_YT_labels_dawson = towns_AK_YT[towns_AK_YT$NAME == 'Dawson City',]

######################################################################################################
######################################################################################################

# 3. PLOT ----------------------------------------------------------------------------

# 3.1 Main map ----------------------------------------------------------------------------

if(season == 'winter'){
  
  main = ggplot(data = ranges_poly_subset) + 
    geom_sf(data = AK_YT, fill = 'white', linewidth = 0.5)+
    geom_sf(data = rivers_AK_YT_filtered, col = 'blue2', linewidth = 0.5)+
    geom_sf(data = roads_AK_YT, linewidth = 0.5)+
    geom_sf(data = towns_AK_YT_filtered, size = 4)+
    geom_sf(aes(fill = year, col = year), alpha = 0.1)+
    geom_sf(data = ranges_centroid_subset, aes(col = year), size = 6)+
    geom_sf(data = core, lty = 'dashed', col = 'black', linewidth = 2, fill = NA)+
    geom_sf_text(data = rivers_AK_YT_labels[rivers_AK_YT_labels$name_en == 'Yukon River',], aes(label = name_en), hjust = -0, vjust = -0.5, col = 'blue2', size = 8)+
    geom_sf_text(data = rivers_AK_YT_labels[rivers_AK_YT_labels$name_en == 'Tanana River',], aes(label = name_en), hjust = 1.3, vjust = -5, col = 'blue2', size = 8)+
    geom_sf_text(data = roads_AK_YT_labels[roads_AK_YT_labels$name == 'Steese Highway',], aes(label = name, vjust = -1), size = 8)+
    geom_sf_text(data = roads_AK_YT_labels[roads_AK_YT_labels$name == 'Alaska Highway',], aes(label = name, vjust = -0.3), size = 8)+
    geom_sf_text(data = towns_AK_YT_labels_main, aes(label = NAME, hjust = 0, vjust = -0.5), size = 8)+
    geom_sf_text(data = towns_AK_YT_labels_dawson, aes(label = NAME, hjust = 0.7, vjust = -0.8), size = 8)+
    theme_grey(base_size = 28)+
    labs(col = 'Year', x = '', y = '')+
    guides(fill="none")+
    viridis::scale_color_viridis(option = "D", breaks = seq(1995, 2020, 5))+ # https://sjmgarnier.github.io/viridis/reference/scale_viridis.html
    viridis::scale_fill_viridis(option = "D")+ # https://sjmgarnier.github.io/viridis/reference/scale_viridis.html
    coord_sf(xlim = c(ext$xmin, ext$xmax), ylim = c(ext$ymin, ext$ymax))+
    annotation_scale(location = "bl", width_hint = 0.3, text_cex = 2, height = unit(0.5, "cm"))+
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                           style = north_arrow_fancy_orienteering)+
    theme(legend.position = c(1, 0.2),
          legend.key.height = unit(1.5, 'cm'),
          legend.key.width = unit(1, 'cm'))
  
}else{
  
  main = ggplot(data = ranges_poly_subset) + 
    geom_sf(data = AK_YT, fill = 'white', linewidth = 0.5)+
    geom_sf(data = rivers_AK_YT_filtered, col = 'blue2', linewidth = 0.5)+
    geom_sf(data = roads_AK_YT, linewidth = 0.5)+
    geom_sf(data = towns_AK_YT_filtered, size = 4)+
    geom_sf(aes(fill = year, col = year), alpha = 0.1)+
    geom_sf(data = ranges_centroid_subset, aes(col = year), size = 6)+
    geom_sf(data = core, lty = 'dashed', col = 'black', linewidth = 2, fill = NA)+
    geom_sf_text(data = rivers_AK_YT_labels, aes(label = name_en), hjust = -0, vjust = -0.5, col = 'blue2', size = 8)+
    geom_sf_text(data = roads_AK_YT_labels, aes(label = name, vjust = -1), size = 8)+
    geom_sf_text(data = towns_AK_YT_labels_main, aes(label = NAME, hjust = 0, vjust = -0.5), size = 8)+
    geom_sf_text(data = towns_AK_YT_labels_dawson, aes(label = NAME, hjust = 0.7, vjust = -0.8), size = 8)+
    theme_grey(base_size = 28)+
    labs(col = 'Year', x = '', y = '')+
    guides(fill="none")+
    viridis::scale_color_viridis(option = "D", breaks = seq(1995, 2020, 5))+ # https://sjmgarnier.github.io/viridis/reference/scale_viridis.html
    viridis::scale_fill_viridis(option = "D")+ # https://sjmgarnier.github.io/viridis/reference/scale_viridis.html
    coord_sf(xlim = c(ext$xmin, ext$xmax), ylim = c(ext$ymin, ext$ymax))+
    annotation_scale(location = "bl", width_hint = 0.3, text_cex = 2, height = unit(0.5, "cm"))+
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                           style = north_arrow_fancy_orienteering)+
    theme(legend.position = c(0.95, 0.5),
          legend.key.height = unit(1.5, 'cm'),
          legend.key.width = unit(1, 'cm'))

}

main

# 3.2 Inset map ----------------------------------------------------------------------------

ext_inset = st_bbox(AK_YT)

inset_map = base_map[(base_map$iso_a2 == 'US' | base_map$iso_a2 == 'CA'),]

inset = ggplot() + 
  geom_sf(data = inset_map, fill = "white", linewidth = 0.5)+ 
  geom_sf(data = st_as_sfc(st_bbox(ext)), fill = NA, col = 'red', linewidth = 1)+ 
  coord_sf(xlim = c(ext_inset$xmin, ext_inset$xmax), ylim = c(ext_inset$ymin, ext_inset$ymax))+
  theme_void()
inset

# 3.3 Join maps ----------------------------------------------------------------------------

final = ggdraw() +
  draw_plot(main) +
  draw_plot(inset, x = 0.7, y = 0.7, width = 0.3, height = 0.3)
final

######################################################################################################
######################################################################################################

# 4. SAVE ----------------------------------------------------------------------------

fig_name_lookup = data.frame(season = c('summer', 'calving', 'postcalving', 'winter'), fig_name = c('fig_4_', 'fig_s4_', 'fig_s5_', 'fig_s6_'))
fig_name = fig_name_lookup$fig_name[fig_name_lookup$season == season]

outName = paste0('annual_range_map_iso', iso_level*100, '_', season, '.jpg')

ggsave(
  paste0(outPath, outName),
  final,
  width = 40,
  height = 30,
  units = 'cm',
  bg = 'white',
  dpi = 600
)
