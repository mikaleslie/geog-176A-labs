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
#remotes::install_github("ropensci/USAboundaries")
#remotes::install_github("ropensci/USAboundariesData")

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

#2.1 - Cities to National Border
state_b %>%
  st_union() %>%
  st_cast("MULTILINESTRING") ->
  us_border

#This left cities with 0 observations
#cities = merge(cities, us_border)

cities = cities %>%
  mutate(dist_to_border = drop_units(set_units(st_distance(.,us_border), "km")))

cities %>%
  dplyr::select(city, state_name, dist_to_border) %>%
  slice_max(dist_to_border, n=5) %>%
  st_drop_geometry() %>%
  knitr::kable(caption = "Cities Farthest from the National Border",
               col.names = c("city", "state_name", "dist_to_border"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

#2.2 - Cities to State Border
state_b %>%
  st_combine() %>%
  st_cast("MULTILINESTRING") ->
  state_border

cities = cities %>%
  mutate(dist_to_stborder = drop_units(set_units(st_distance(.,state_border), "km")))

cities %>%
  dplyr::select(city, state_name, dist_to_stborder) %>%
  slice_max(dist_to_stborder, n=5) %>%
  st_drop_geometry() %>%
  knitr::kable(caption = "Cities Farthest from a State Border",
               col.names = c("city", "state_name", "dist_to_stborder"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

#2.3 - Cities to Mexican Border
mexico = country_b %>%
  filter(sovereignt == "Mexico")

cities = cities %>%
  mutate(dist_to_mexborder = drop_units(set_units(st_distance(., mexico), "km")))

cities %>%
  dplyr::select(city, state_name, dist_to_mexborder) %>%
  slice_max(dist_to_mexborder, n=5) %>%
  st_drop_geometry() %>%
  knitr::kable(caption = "Cities Farthest from the Mexican Border",
               col.names = c("city", "state_name", "dist_to_mexborder"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

#2.4 - Cities to Canadian Border
canada = country_b %>%
  filter(sovereignt == "Canada")

cities = cities %>%
  mutate(dist_to_canborder = drop_units(set_units(st_distance(., canada), "km")))

cities %>%
  dplyr::select(city, state_name, dist_to_canborder) %>%
  slice_max(dist_to_canborder, n=5) %>%
  st_drop_geometry() %>%
  knitr::kable(caption = "Cities Farthest from the Canadian Border",
               col.names = c("city", "state_name", "dist_to_canborder"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

#3.1 Data
big_cities = cities %>%
  slice_max(population, n = 10)

ggplot() +
  geom_sf(data = filter(country_b, sov_a3 != "US1")) +
  geom_sf(data = state_b) +
  geom_sf(data = big_cities, col = "red")+
  ggrepel::geom_label_repel(data = big_cities,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 1.5)+
  labs(x = "",
       y = "")

#3.2 Distance from the National Border
ggplot() +
  geom_sf(data = filter(country_b, sov_a3 != "US1")) +
  geom_sf(data = cities,
          aes(col = dist_to_border),
          size = 0.1) +
  geom_sf(data = state_border, alpha = 0.7, lty = 3)+
  geom_sf(data = slice_max(cities, dist_to_border, n=5), col = "red", size = 0.75)+
  ggrepel::geom_label_repel(data = slice_max(cities, dist_to_border, n=5),
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 2)

#3.3 Distance from the Nearest State Border
ggplot() +
  geom_sf(data = filter(country_b, sov_a3 != "US1")) +
  geom_sf(data = cities,
          aes(col = dist_to_stborder),
          size = 0.1) +
  geom_sf(data = state_border, alpha = 0.7, lty = 3)+
  geom_sf(data = slice_max(cities, dist_to_stborder, n=5), col = "red", size = 0.75)+
  ggrepel::geom_label_repel(data = slice_max(cities, dist_to_stborder, n=5),
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 2)+
  labs(x = "",
       y = "")

#3.4 Equidistance from Canada and Mexico
cities = cities %>%
  mutate(canmex_diff = abs(dist_to_canborder - dist_to_mexborder))

ggplot() +
  geom_sf(data = filter(country_b, sov_a3 != "US1")) +
  geom_sf(data = cities, col = "red", alpha = 0.5, size = 0.1) +
  gghighlight::gghighlight(canmex_diff <= 100)+
  geom_sf(data = (cities %>%
                    filter(canmex_diff <= 100) %>%
                    slice_max(population, n=5)),
          col = "blue", size = 0.1) +
  geom_sf(data = state_border, alpha = 0.7, lty = 3)+
  ggrepel::geom_label_repel(data = sub_cities,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 2)+
  labs(x = "",
       y = "")

#4.1
cities %>%
  filter(dist_to_border <= 160) %>%
  nrow()

cities = cities %>%
  mutate(total_pop = sum(population))

new = cities %>%
  filter(dist_to_border <= 160) %>%
  mutate(coast_pop = sum(population)) %>%
  mutate(percent_pop = (coast_pop/total_pop)*100)

new = new %>%
  mutate(cities_count = nrow(new))

new%>%
  head(1) %>%
  dplyr::select("cities_count", "coast_pop", "percent_pop") %>%
  st_drop_geometry() %>%
  knitr::kable(caption = "US Border Zone and Populations",
               col.names = c("cities_count", "coast_pop", "percent_pop"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

#4.2
#10 Most Populous Cities in Danger Zone
ggplot() +
  geom_sf(data = state_b)+
  geom_sf(data = cities, aes(col = dist_to_border), alpha = 0.5, size = 0.1) +
  gghighlight(dist_to_border <= 160)+
  scale_color_gradient(low = "darkred", high = "orange")+
  geom_sf(data = slice_max(new, population, n = 10), size = 0.1)+
  ggrepel::geom_label_repel(data = slice_max(new, population, n = 10),
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 2)+
  labs(x = "",
       y = "")




#Most Populous City in Every State
new_state = new %>%
  group_by(state_name) %>%
  slice_max(population, n=1)

ggplot() +
  geom_sf(data = state_b)+
  geom_sf(data = cities, aes(col = dist_to_border), alpha = 0.5, size = 0.1) +
  gghighlight(dist_to_border <= 160)+
  scale_color_gradient(low = "darkred", high = "orange")+
  geom_sf(data = new_state,
          size = 0.1)+
  ggrepel::geom_label_repel(data = new_state,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 2)+
  labs(x = "",
       y = "")












