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
library(rstatix)

# 1.2 Parameters ---------------------------------------------------------------

vars = c('eastness', 'elev', 'northness', 'snow', 'tpi', 'tri')

inPath = 'data/env_data/'
outPath = 'data/env_data/ks_results/'

################################################################################
################################################################################

# 2. LOOP VARIABLES ------------------------------------------------------------

ks_df_all = data.frame()

for(var in vars){
  
  print(paste0('Current variable: ', var))
  
  ##############################################################################
  ##############################################################################
  
  # 3. READ IN AND TIDY DATA ---------------------------------------------------
  
  # 3.1 Read in and format -----------------------------------------------------
  
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
  
  # 3.2 Combine and tidy -------------------------------------------------------
  
  # Combine
  df = bind_rows(calving, postcalving, summer, winter, overall)
  
  # Tidy
  df = dplyr::select(df, -any_of(c('system.index', '.geo')))
  df = na.omit(df)
  names(df) = gsub('[.]', '_', names(df))
  
  # Recode caribou density classes
  df$caribou_density = as.factor(df$caribou_density)
  df$caribou_density = recode(df$caribou_density, `1` = 'None', `2` = 'Low', `3` = 'Medium', `4` = 'High')
  
  # 3.3 Subset for plotting ------------------------------------------------------
  
  df = df[df$season %in% c('Calving', 'Post-calving', 'Summer', 'Winter', 'Overall'),]
  df$season = factor(df$season, levels = c('Calving', 'Post-calving', 'Summer', 'Winter', 'Overall'))
  
  ##############################################################################
  ##############################################################################
  
  # 4. K-S TESTS ---------------------------------------------------------------
  
  # https://stackoverflow.com/questions/56361724/multiple-tests-with-pairwise-combinations-using-dplyr-tidyverse
  
  # 4.1 Format factors as characters -------------------------------------------
  
  df$caribou_density = as.character(df$caribou_density)
  df$season = as.character(df$season)
  
  # 4.2 Get all unique combinations of caribou density classes -----------------
  
  combos = unique(df$caribou_density) %>% # Get classes
    combn(2, simplify = F) %>% # Create combinations
    set_names(map_chr(., ~ paste(., collapse = "_"))) # Assign combo names
  
  # 4.3 Perform Kolmogorov-Smirnov tests ---------------------------------------
  # Grouped by season
  # Pairwise across caribou density classes
  
  ks_df = df %>% 
    # Group by season, creating list of tibbles rather than grouped data.frame
    group_split(season) %>% 
    # Name each tibble with matching season
    set_names(map_chr(., ~ unique(.$season))) %>% 
    # Map over tibbles, returning a data.frame
    map_df(function(x) { 
      # Map over combos, returning a data.frame
      map_df(combos, function(y) {
        # Filter seasonal data to only density classes in current combo
        filter(x, caribou_density %in% y) %>% 
          # Perform K-S test
          ks.test(get(var) ~ caribou_density, data = .) %>% 
          broom::tidy()
      }, .id = "combo") # Assign combo name to results
    }, .id = "season") # Assign season name to results

  # 4.4 Add to cumulative data.frame -------------------------------------------
  
  ks_df$env_var = var
  ks_df_all = bind_rows(ks_df_all, ks_df)
  
}

################################################################################
################################################################################

# 5. SAVE ----------------------------------------------------------------------

write.csv(ks_df_all, paste0(outPath, 'ks_tests_density_season_env.csv'), row.names = FALSE)