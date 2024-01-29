######################################################################################################
######################################################################################################

# CODE DESCRIPTION

# This script plots time series of relevant climate variables over the study period, study area, and background areas

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

######################################################################################################
######################################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/climate_data/'

outPath = 'figures/'

######################################################################################################
######################################################################################################

# 1. SET UP ----------------------------------------------------------------------------

# 1.1 Parameters ----------------------------------------------------------------------------

# Set color palette
colorblind = c("#009E73", "#E69F00", "#999999") # teal, orange, grey

# Slope formula
formula <- y ~ x
myformat <- "Slope: %s"

# 1.2 Packages ----------------------------------------------------------------------------

library(tidyverse)
library(ggpmisc)

# 1.3 Read in data ----------------------------------------------------------------------------

climate = read.csv(paste0(inPath, 'climate_time_series.csv'))

######################################################################################################
######################################################################################################

# 2. TIDY ----------------------------------------------------------------------------

# Remove unnecessary columns
climate = dplyr::select(climate, -c(system.index, FiveYrBlck, One, WinterYr, .geo, season))

# Scale climate variables
climate[,grepl("aet", colnames(climate))] = climate[,grepl("aet", colnames(climate))]*0.1
climate[,grepl("def", colnames(climate))] = climate[,grepl("def", colnames(climate))]*0.1
climate[,grepl("pdsi", colnames(climate))] = climate[,grepl("pdsi", colnames(climate))]*0.01
climate[,grepl("pet", colnames(climate))] = climate[,grepl("pet", colnames(climate))]*0.1
climate[,grepl("soil", colnames(climate))] = climate[,grepl("soil", colnames(climate))]*0.1
climate[,grepl("srad", colnames(climate))] = climate[,grepl("srad", colnames(climate))]*0.1
climate[,grepl("tmmn", colnames(climate))] = climate[,grepl("tmmn", colnames(climate))]*0.1
climate[,grepl("tmmx", colnames(climate))] = climate[,grepl("tmmx", colnames(climate))]*0.1
climate[,grepl("tmavg", colnames(climate))] = climate[,grepl("tmavg", colnames(climate))]*0.1
climate[,grepl("swi", colnames(climate))] = climate[,grepl("swi", colnames(climate))]*0.1
climate[,grepl("vap", colnames(climate))] = climate[,grepl("vap", colnames(climate))]*0.001
climate[,grepl("vpd", colnames(climate))] = climate[,grepl("vap", colnames(climate))]*0.01
climate[,grepl("vs", colnames(climate))] = climate[,grepl("vs", colnames(climate))]*0.01

# Pivot longer
climate_long = pivot_longer(climate, !c(range, year), names_to = 'climate_variable', values_to = 'value')

# Separate metrics
climate_long$climate_variable = gsub('swi', 'swi_mean', climate_long$climate_variable)
climate_long = separate(climate_long, climate_variable, c('climate_variable', 'annual_aggregation', 'metric'))
climate_long$annual_aggregation = tools::toTitleCase(climate_long$annual_aggregation)
climate_long$climate_variable = paste0(climate_long$climate_variable, climate_long$annual_aggregation)
climate_long = dplyr::select(climate_long, -c(annual_aggregation))

# Pivot wider
climate_long = pivot_wider(climate_long, names_from = metric, values_from = value)

# Calculate confidence intervals
climate_long$se = climate_long$stdDev/sqrt(climate_long$count)
climate_long$lwr = climate_long$mean - (1.96 * climate_long$se)
climate_long$upr = climate_long$mean + (1.96 * climate_long$se)

#Subset data
data = climate_long[(climate_long$range == 'core' | climate_long$range == 'full' | climate_long$range == 'ak_yt'),] # Full
data = data[(data$climate_variable == 'prSum' | data$climate_variable == 'swiMean' | data$climate_variable == 'tmavgMean' | data$climate_variable == 'tmmnMin' | data$climate_variable == 'tmmxMax'),]

# Rename data
data$range = gsub('ak_yt', 'Alaska, Yukon', data$range)
data$range = gsub('core', 'Core uplands', data$range)
data$range = gsub('full', 'Extended range', data$range)

# Assign and order factors
data$range = factor(data$range, levels=c('Core uplands', 'Extended range', 'Alaska, Yukon'))
data$climate_variable <- factor(
  data$climate_variable,
  levels = c("prSum", "swiMean", "tmavgMean", "tmmnMin", "tmmxMax"),
  labels = c(
    "Precipitation~(mm)", 
    "Summer~warmth~index~(degree*C)", 
    "Average~temperature~(degree*C)",
    "Minimum~temperature~(degree*C)",
    "Maximum~temperature~(degree*C)"
  ))

######################################################################################################
######################################################################################################

# 3. PLOT ----------------------------------------------------------------------------

plt = 
  ggplot(data, aes(x = as.numeric(year), y = mean, col = range))+
  geom_point(size = 3)+
  facet_wrap(~as.factor(climate_variable), scales = "free", labeller=label_parsed, ncol = 2)+
  theme_minimal(base_size = 24)+
  labs(y = '', x = "", fill = 'Extent', col = 'Extent')+
  stat_poly_line(formula = formula, method = 'lm')+
  stat_poly_eq(formula = formula,
               output.type = "numeric",
               parse = TRUE,
               size = 6,
               mapping = aes(label = sprintf(myformat, round(after_stat(b_1), 2))))+
  theme(legend.position = c(0.9, 0),
        legend.justification = c(1, 0),)+
  guides(fill="none")+
  scale_color_manual(values = colorblind)

plt

######################################################################################################
######################################################################################################

# 4. SAVE ----------------------------------------------------------------------------

outName = 'fig_s1_climate_time_series.jpg'

ggsave(
  paste0(outPath, outName),
  plt,
  width = 30,
  height = 40,
  units = 'cm',
  bg = 'white',
  dpi = 600
)

