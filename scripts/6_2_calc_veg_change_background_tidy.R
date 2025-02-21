################################################################################
################################################################################

# CODE DESCRIPTION

# This script tidies data quantifying levels of PFT change across background areas

# CODE AUTHOR: Kathleen Orndahl
# CONTACT: kathleen.orndahl@nau.edu

################################################################################
################################################################################

# SET OUTPUT DIRECTORY

inPath = 'data/veg_data/weighted_area/'

outPath = 'data/veg_data/weighted_area/'

################################################################################
################################################################################

# 1. SET UP --------------------------------------------------------------------

# 1.1 Packages -----------------------------------------------------------------

library(tidyverse)

# 1.2 Read in data -------------------------------------------------------------

df = read.csv(paste0(inPath, 'pft_change_background.csv'))

################################################################################
################################################################################

# 2. TIDY ----------------------------------------------------------------------

# Remove unnecesary columns and NAs
df = subset(df, select = -c(.geo, system.index))
df = na.omit(df)

# Format data
df = df %>% pivot_longer(!c(area, name), 
                         names_pattern = "(.*)_(.*)", 
                         names_to = c("pft", ".value")) %>%
            separate_wider_delim(pft, '_', names = c('pft', 'year'))

# Calculate percent coverage
df$area_km2 = df$area/1000000
df$percent_cover = (df$cover/df$area)*100

# Format data
df = df %>% pivot_wider(names_from = year, values_from = c(cover, percent_cover))

# Calculate change
df$percent_change = round(df$percent_cover_2020, 1) - round(df$percent_cover_1992, 1)
df$relative_percent_change = round((df$percent_change/round(df$percent_cover_1992, 1))*100, 1)

################################################################################
################################################################################

# 3. SAVE ----------------------------------------------------------------------

write.csv(df, paste0(outPath, 'pft_change_background_final.csv'), row.names = FALSE)
