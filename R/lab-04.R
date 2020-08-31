# SPDS
library(tidyverse)
library(sf)
library(units)

counties = USAboundaries::us_counties() %>%
  filter(!(state_name %in% c("Puerto Rico", "Alaska", "Hawaii"))) %>%
  st_transform(5070)

#Get centroids
county_cent = st_centroid(counties) %>%
  st_combine()

  #Make voroni
v_grid = st_voronoi(county_cent) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

v_grid = st_intersection(v_grid, st_union(counties))
plot(v_grid)

#Make triangulation
t_grid = st_triangulate(county_cent) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

t_grid = st_intersection(t_grid, st_union(counties))
plot(t_grid)

#Make gridded coverage
sq_grid = st_make_grid(county_cent, n = c(70, 70)) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

#Make hexagonal coverage
hex_grid = st_make_grid(county_cent, n = c(70, 70), square = FALSE) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

#Simplify feature boundary
simp_counties = st_union(counties)

simp_counties = simp_counties %>%
  rmapshaper::ms_simplify(keep = 0.1)


  plot(simp_counties)

#Number of points in original object:
mapview::npts(st_union(counties))

#Number of points in simplified object:
mapview::npts(simp_counties)

#Difference in points
paste("The difference in points between the original object and the simplified object is", mapview::npts(st_union(counties)) - mapview::npts(simp_counties))

#Crop triangulated tesselations
v_grid = st_intersection(v_grid, simp_counties)
t_grid = st_intersection(t_grid, simp_counties)

#Ploting tesselations function
plot_tess = function(data, title){
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = .2) +
    theme_void() +
    labs(title = title, caption = paste("This tesselation has:", nrow(data), "tiles" )) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}

#Make ggplots
plot_tess(counties, "Original County Data")
plot_tess(v_grid, "Voronoi Coverage")
plot_tess(t_grid, "Triangulation Coverage")
plot_tess(sq_grid, "Square Grid Coverage")
plot_tess(hex_grid, "Hexagonal Grid Coverage")

### Question 2
#2.1


sum_tess = function(data, title){
    newdata = data %>%
      mutate(number_of_features = nrow(data)) %>%
      mutate(mean_area = mean(drop_units(set_units(st_area(data), "km2")))) %>%
      mutate(sd_area = sd(drop_units(set_units(st_area(data), "km2")))) %>%
      mutate(total_area = mean_area * number_of_features)

newdata$tesselation <- title

newdata = newdata %>%
  select(tesselation, number_of_features, mean_area, sd_area, total_area) %>%
  st_drop_geometry() %>%
  head(1)

return(newdata)
}

#Testing out code to put in function
newcounties = counties %>%
  mutate(number_of_features = nrow(counties)) %>%
  mutate(mean_area = mean(drop_units(set_units(st_area(counties), "km2")))) %>%
  mutate(sd_area = sd(drop_units(set_units(st_area(counties), "km2")))) %>%
  mutate(total_area = sum(drop_units(set_units(st_area(counties), "km2"))))

newcounties$tesselation <- "Original Counties"

newcounties %>%
  select(tesselation, number_of_features, mean_area, sd_area, total_area) %>%
  st_drop_geometry() %>%
  head(1)
#This appears to work fine like this, but prompted an error when I tried to use
#it as a function

# 2.2 - tried this and received an error: "Can't subset columns that don't exist."
sum_tess(counties, "Original Counties")
sum_tess(v_grid, "Voroni")
sum_tess(t_grid, "Triangulation")
sum_tess(sq_grid, "Square Grid")
sum_tess(hex_grid, "Hexagonal Grid")

# 2.3
tess_summary = bind_rows(
  sum_tess(counties, "Original Counties"),
  sum_tess(v_grid, "Voroni"),
  sum_tess(t_grid, "Triangulation"),
  sum_tess(sq_grid, "Square Grid"),
  sum_tess(hex_grid, "Hexagonal Grid"))

# 2.4
tess_summary %>%
  knitr::kable(caption = "Tesselation Coverage of the US",
               col.names = c("tesselation", "number_of_features", "mean_area", "sd_area", "total_area"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)


#Question 3

#3.1
library(readxl)
dams <- read_excel("data/NID2019_U.xlsx")

# Filter isn't working and I'm not sure why
dams = dams %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(5070)

#3.2
point_in_polygon = function(points, polygon, group){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(group)) %>%
    setNames(c(group, "n")) %>%
    left_join(polygon, by = group) %>%
    st_as_sf()
}

#3.3

counties = point_in_polygon(dams, counties, "geoid")
v_grid = point_in_polygon(dams, v_grid, "id")
t_grid = point_in_polygon(dams, t_grid, "id")
sq_grid = point_in_polygon(dams, sq_grid, "id")
hex_grid = point_in_polygon(dams, hex_grid, "id")

#3.4

plot_point_tess = function(data, title){
  ggplot() +
    geom_sf(data = data, aes(fill = n), col = NA, size = .2) +
    scale_fill_viridis_c("Number of Dams")+
    theme_void() +
    labs(title = title,
         caption = paste0(sum(data$n), " dams represented")) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}

#3.5

plot_point_tess(counties, "Dam Densities across Counties")
plot_point_tess(v_grid, "Dam Densities across a Voronoi Tesselation")
plot_point_tess(t_grid, "Dam Densities across a Triangulated Tesselation")
plot_point_tess(sq_grid, "Dam Densities across a Square Grid")
plot_point_tess(hex_grid, "Dam Densities across a Hexagonal Grid")

#3.6

# Question 4



#4.1 - R, C, P, S
r_dams = dams %>%
  filter(grepl("R", dams$PURPOSES) == TRUE)

c_dams = dams %>%
  filter(grepl("C", dams$PURPOSES) == TRUE)

p_dams = dams %>%
  filter(grepl("P", dams$PURPOSES) == TRUE)

s_dams = dams %>%
  filter(grepl("S", dams$PURPOSES) == TRUE)

#drop previous n column from v_grid
v_grid = v_grid %>%
  select(id, x)

#use pip to count subset across tesselation
rdam_v_grid = point_in_polygon(r_dams, v_grid, "id")
cdam_v_grid = point_in_polygon(c_dams, v_grid, "id")
pdam_v_grid = point_in_polygon(p_dams, v_grid, "id")
sdam_v_grid = point_in_polygon(s_dams, v_grid, "id")

#4.2
plot_point_tess(rdam_v_grid, "Dams used for Recreation")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))
plot_point_tess(cdam_v_grid, "Dams used for Flood Control")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))
plot_point_tess(pdam_v_grid, "Dams used for Fire Protection")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))
plot_point_tess(sdam_v_grid, "Dams used for Water Supply")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))

#or should we do it like this?
point_in_polygon(r_dams, v_grid, "id") %>%
  plot_point_tess("Dams used for Recreation")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))

#Extra Credit
river = read_sf("data/MajorRivers.shp") %>%
  filter(SYSTEM == "Mississippi")

library(leaflet)
library(USAboundaries)

#Filter dams - this only creates 1 row because all STATEIDs = NA
large_dams = dams %>%
  select(DAM_NAME, NID_STORAGE, PURPOSES, YEAR_COMPLETED, HAZARD, STATE) %>%
  filter(HAZARD == "H") %>%
  group_by(STATE) %>%
  slice_max(NID_STORAGE, n=1)

leaflet(data = large_dams) %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(data = large_dams, fill = "red", fillOpacity = .5, stroke = FALSE)



