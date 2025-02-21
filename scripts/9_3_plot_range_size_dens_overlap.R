######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script creates figure showing time series of Fortymile caribou seasonal range size, density and overlap with Core Uplands

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPathIso = 'data/range_data/isopleths/0_season_year/0_orig/'
inPathPop = 'data/pop_data/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Set parameters ----------------------------------------------------------------------------

# Set isopleth type for seasonal-year ranges
iso_level = 0.5 # Choose 0.95 or 0.5

# 1.2 Packages ----------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(ggpmisc)
library(units)
library(scales)

# 1.3 Read in data ----------------------------------------------------------------------------

# Get season-year ranges
iso = read.csv(paste0(inPathIso, 'iso_all_overlap.csv'))

# Get population data
population = read.csv(paste0(inPathPop, 'fmch_population_size.csv'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# 2.1 Population data ----------------------------------------------------------------------------

# Remove repetitive data
population = population[!(population$year>=2009 & population$rivest_adjustment == 'N'),] # Remove non-Rivest data for later years
population = population[!(population$year==2009 & population$estimate_type == 'model'),] # Remove modeled estimate for years where there is a photocensus
population = population[!(population$year==2017 & population$estimate_type == 'model'),] # Remove modeled estimate for years where there is a photocensus
population = dplyr::select(population, c(year, population_size))

# 2.2 Range data ----------------------------------------------------------------------------

# Get as range data as a dataframe
df = iso %>% st_drop_geometry()
df$year = as.numeric(df$year)
df$year = as.numeric(df$year)
df$year = as.numeric(df$year)
df$year = as.numeric(df$year)
df$year = as.numeric(df$year)
df$year = as.numeric(df$year)

######################################################################################################
######################################################################################################

# 3. CALCULATE PROPERTIES ----------------------------------------------------------------------------

# Add population sizes to range data
# NOTE: no population data for 2021, that is okay
df = dplyr::left_join(df, population, by = 'year')

# Calculate density
df$density = (df$population_size * df$level)/df$area_km
df$density = as.numeric(df$density)

# Add percent overlap
df$percent_overlap = (df$int_km2/df$area_km)*100

######################################################################################################
######################################################################################################

# 4. PLOT ----------------------------------------------------------------------------

# Select isopleth level
df_plot = df[df$level == iso_level,]

# Pivot wider
df_plot_all = tidyr::pivot_longer(df_plot, cols = c('area_km', 'density', 'percent_overlap'), names_to = 'response_type', values_to = 'values')

# Subset to desired seasons
df_plot_all = df_plot_all[(df_plot_all$season == 'calving'|df_plot_all$season == 'postcalving'|df_plot_all$season == 'summer'|df_plot_all$season == 'winter'),]

# Format properties
df_plot_all$response_type = factor(
  df_plot_all$response_type,
  labels = c(
    "Area~(km^{2})", 
    "atop(Caribou~density,(animals~per~km^{2}))",
    "atop(Percent~overlap, with~Core~Uplands)"
  ))

# Create dummy data frame to ensure some space is left at the top of the percent overlap facets
dummy = data.frame(season = unique(df_plot_all$season), year = 1990, response_type = "atop(Percent~overlap, with~Core~Uplands)", values = 120)

# Set up stat_poly equations
formula = y ~ x
myformat = "Slope: %s"

# Plot
plt = ggplot(df_plot_all, aes(x = year, y = values))+
  geom_line(size = 1, alpha = 0.5)+
  geom_point(size = 2)+
  geom_blank(data = dummy, aes(x = year, y = values))+
  facet_grid(response_type~season, scales="free_y", labeller=label_parsed, switch = "y")+
  stat_poly_line(formula = formula, method = 'lm')+
  stat_poly_eq(formula = formula,
               output.type = "numeric",
               parse = TRUE,
               size = 8,
               mapping = aes(label = sprintf(myformat, round(after_stat(b_1), 2))),
               label.y = 1)+ # Plot slope
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  label.x = 0.05, label.y = 0.92, size = 8,
                  aes(label = sprintf("italic(P)*\" = \"*%.3g", 
                                      signif(..p.value.., digits = 2))),
                  parse = TRUE)+ # Plot P-value
  stat_poly_eq(formula = formula, label.y = 0.87, size = 8)+ # Plot R2
  theme_minimal(base_size = 34)+
  theme(strip.placement = "outside",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.y.left =  element_text(vjust = -0.25),
        plot.margin = margin(5.5, 5.5, 5.5, -50, "pt"))+
  scale_y_continuous(breaks = scales::pretty_breaks(4))+
  labs(x = 'Year', y = '')
plt

######################################################################################################
######################################################################################################

# 5. SAVE ----------------------------------------------------------------------------

if(iso_level == 0.95){
  fig_name = 'fig_3_'
}else{
  fig_name = 'fig_s3_'
}

outName = paste0(fig_name, 'range_size_density_overlap_iso', iso_level*100, '.jpg')

ggsave(
  paste0(outPath, outName),
  plt,
  width = 40,
  height = 40,
  units = 'cm',
  bg = 'white',
  dpi = 600
)
