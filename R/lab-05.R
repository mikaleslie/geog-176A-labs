library(tidyverse)
library(sf)
library(raster)

library(getlandsat)
library(mapview)
library(osmdata)


#Question 1
bb = read_csv("data/uscities.csv") %>%
  filter(city == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()
#bb = AOI for this analysis

#Question 2
#2.1
scenes = getlandsat::lsat_scenes()

bbwgs = bb %>% st_transform(4326)
bb2 = st_bbox(bbwgs)

down = scenes %>%
  filter(min_lat <= bb2$ymin, max_lat >= bb2$ymax,
         min_lon <= bb2$xmin, max_lon >= bb2$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(down, file = "data/palo-flood-scene.csv", row.names = F)

#2.2
osm = osmdata::opq(bbwgs) %>%
  add_osm_feature("building")


files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0("B", 1:6, ".TIF$", collapse = "|"), file)) %>%
  arrange(file) %>%
  pull(file)
