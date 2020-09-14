##################################
## Project: Lab 03
## Script Purpose: Week 3
## Date: August 20
##################################

# SPDS
library(tidyverse)
library(sf)
library(units)

# Data
library(USAboundaries)
library(rnaturalearth)

# Visualization
library(gghighlight)
library(ggrepel)
library(knitr)

#1.1 - Define projection
eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

#1.2 - US Boundaries
# ONLY NEED TO INSTALL ONCE :)

# remotes::install_github("ropensci/USAboundaries")
# remotes::install_github("ropensci/USAboundariesData")

state_b = USAboundaries::us_states(resolution = "low") %>%
  filter(!(state_name %in% c("Puerto Rico", "Alaska", "Hawaii"))) %>%
  st_transform(eqdc)

#1.3 - Country Borders
country_b = st_as_sf(rnaturalearth::countries110) %>%
  filter(admin %in% c("United States of America", "Mexico", "Canada")) %>%
  st_transform(eqdc)

#1.4 - US Cities
cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(!(state_name %in% c("Puerto Rico", "Alaska", "Hawaii"))) %>%
  st_transform(eqdc)

#2.1 - City to National Border
state_b %>%
  st_union() %>%
  st_cast("MULTILINESTRING") ->
  us_border

#This left cities with 0 observations

# THIS FAILS BECAUSE THERE IN NO COMMON KEY BETWEEN THE TWO
#cities = merge(cities, us_border)

### MIKE ADD ###

## So the idea is to add the the distance as an element of cites since there is one distance per city. You could repeat this for the other four tests!

cities = cities %>%
  mutate(dist_to_border = drop_units(set_units(st_distance(.,us_border), "km")))

# Your table below is then perfect!

cities %>%
  dplyr::select(city, state_name, dist_to_border) %>%
  slice_max(dist_to_border, n = 5) %>%
  st_drop_geometry() %>%
  knitr::kable(caption = "Cities Farthest from the National Border",
               col.names = c("city", "state_name", "dist_Cborder"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)
################

cities %>%
  mutate(dist_Cborder = st_distance(us_border)) %>%
  slice_max(dist_Cborder, n=5) %>%
  select("city", "state_name", "dist_Cborder") %>%
  knitr::kable(caption = "Cities Farthest from the National Border",
               col.names = c("city", "state_name", "dist_Cborder"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)







# install.packages("ggrepel")
# install.packages("gghighlight")
