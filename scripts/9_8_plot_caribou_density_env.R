################################################################################
################################################################################

# CODE DESCRIPTION

# This script plots density distributions for several environmental variables
# across caribou density classes:
# - Eastness
# - Elevation
# - Northness
# - Snow-free day-of-year
# - Soil nitrogen at: 0-5 cm, 5-15 cm, 15-30 cm
# - Topographic position index
# - Topographic ruggedness index

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

################################################################################
################################################################################

# 1. SET UP --------------------------------------------------------------------

# 1.1 Packages -----------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(MASS)

# 1.2 Parameters ---------------------------------------------------------------

vars = c('eastness', 'elev', 'northness', 'snow', 'nitrogen_0_5cm_mean', 'nitrogen_5_15cm_mean', 'nitrogen_15_30cm_mean', 'tpi', 'tri')
var_label_lookup = data.frame(var = c('eastness', 'elev', 'northness', 'snow', 'nitrogen_0_5cm_mean', 'nitrogen_5_15cm_mean', 'nitrogen_15_30cm_mean', 'tpi', 'tri'),
                              label = c('Eastness (radians)', 'Elevation (m)', 'Northness (radians)', 'Snow-Free Day-of-Year', 'Soil Nitrogen, 0-5 cm (g/kg)', 'Soil Nitrogen, 5-15 cm (g/kg)', 'Soil Nitrogen, 15-30 cm (g/kg)', 'Topographic Position Index', 'Topographic Ruggedness Index'))
var_fig_lookup = data.frame(var = c('eastness', 'elev', 'northness', 'snow', 'nitrogen_0_5cm_mean', 'nitrogen_5_15cm_mean', 'nitrogen_15_30cm_mean', 'tpi', 'tri'),
                            label = c('fig_s7', 'fig_s8', 'fig_s9', 'fig_s10', 'fig_s24', 'fig_s25', 'fig_s26', 'fig_s11', 'fig_s12'))
dens_pal = c("green4", "yellow", "orange", "red")
rel_min_height = 0.01

inPath = 'data/env_data/'
outPath = 'figures/'

################################################################################
################################################################################

for(var in vars){
  
  print(paste0('Current variable: ', var))
  
  # 2. READ IN AND TIDY DATA ---------------------------------------------------
  
  # 2.1 Read in and format -----------------------------------------------------
  
  var_in = if(grepl('nitrogen', var)) 'soil' else var
  
  calving = read.csv(paste0(inPath, 'density_', var_in, '_distribution10_calving.csv'))
  postcalving = read.csv(paste0(inPath, 'density_', var_in, '_distribution10_postcalving.csv'))
  summer = read.csv(paste0(inPath, 'density_', var_in, '_distribution10_summer.csv'))
  winter = read.csv(paste0(inPath, 'density_', var_in, '_distribution10_winter.csv'))
  overall = read.csv(paste0(inPath, 'density_', var_in, '_distribution10_overall.csv'))
  
  # Format season names
  calving$season = 'Calving'
  postcalving$season = 'Post-calving'
  summer$season = 'Summer'
  winter$season = 'Winter'
  overall$season = 'Overall'
  
  # 2.2 Combine and tidy -------------------------------------------------------
  
  # Combine
  df = bind_rows(calving, postcalving, summer, winter, overall)
  
  # Tidy
  df = dplyr::select(df, -any_of(c('system.index', '.geo')))
  df = na.omit(df)
  names(df) = gsub('[.]', '_', names(df))
  
  # Recode caribou density classes
  df$caribou_density = as.factor(df$caribou_density)
  df$caribou_density = recode(df$caribou_density, `1` = 'None', `2` = 'Low', `3` = 'Medium', `4` = 'High')
  
  # 2.3 Subset for plotting ----------------------------------------------------
  
  df = df[df$season %in% c('Calving', 'Post-calving', 'Summer', 'Winter', 'Overall'),]
  df$season = factor(df$season, levels = c('Calving', 'Post-calving', 'Summer', 'Winter', 'Overall'))
  
  ##############################################################################
  ##############################################################################
  
  # 3. PLOT --------------------------------------------------------------------

  # 3.1 Calculate bandwidth ----------------------------------------------------
  
  # Perform grouped random sample to facilitate bandwidth calculation
  # Not necessary for soil data which has coarser spatial resolution and thus smaller original sample size
  df_sub = if(var_in == 'soil') df else df %>% group_by(caribou_density) %>% slice_sample(prop = 0.01)
  
  # Calculate bandwidth
  bw = bcv(df_sub[[var]])
  
  # 3.2 Plot -------------------------------------------------------------------
  
  plt = ggplot(df, aes(x = !!sym(var), y = caribou_density, fill = caribou_density))+
    geom_density_ridges(bandwidth = bw, rel_min_height = rel_min_height)+
    facet_wrap(~season)+
    scale_fill_manual(values = dens_pal)+
    theme_minimal(base_size = 40)+
    theme(axis.text.x = element_text(size = 20))+
    guides(fill="none")+
    labs(x = var_label_lookup$label[var_label_lookup$var == var], y = "Caribou relative spatial density")
  
  # 3.3 Extract tail cutoff ----------------------------------------------------
  
  plt_bld = ggplot_build(plt)
  plt_bld_data = plt_bld$data[[1]]
  height_cutoff = max(plt_bld_data$height) * rel_min_height
  plotted_data = plt_bld_data[plt_bld_data$height > height_cutoff,]
  xmin_plt = min(plotted_data$x)
  xmax_plt = max(plotted_data$x)
  
  # 3.4 Plot with proper bounds ------------------------------------------------
  
  plt = ggplot(df, aes(x = !!sym(var), y = caribou_density, fill = caribou_density))+
    geom_density_ridges(bandwidth = bw, rel_min_height = rel_min_height, alpha = 0.8)+
    facet_wrap(~season)+
    scale_fill_manual(values = dens_pal)+
    theme_minimal(base_size = 40)+
    theme(axis.text.x = element_text(size = 20))+
    guides(fill="none")+
    xlim(c(xmin_plt, xmax_plt))+
    labs(x = var_label_lookup$label[var_label_lookup$var == var], y = "Caribou relative spatial density")
  plt
  
  ##############################################################################
  ##############################################################################
  
  # 4. SAVE --------------------------------------------------------------------
  
  outName = paste0(var_fig_lookup$label[var_fig_lookup$var == var], '_caribou_density_', var, '.jpg')
  
  ggsave(
    paste0(outPath, outName),
    plt,
    width = 40,
    height = 30,
    units = 'cm',
    bg = 'white',
    dpi = 300
  )

}
