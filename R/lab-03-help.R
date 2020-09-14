region = data.frame(region = state.region,
                    state_name = state.name)

south = USAboundaries::us_states() %>%
  right_join(region, by = "state_name") %>%
  filter(region == "South")

cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_filter(south, .predicate = st_intersects)

south_c = st_combine(south) %>%
  st_cast("MULTILINESTRING")

south_c = st_transform(south_c, 5070)
cities = st_transform(cities, 5070)

cities = cities %>%
  mutate(dist_state = st_distance(cities, south_c),
         dist_state = units::set_units(dist_state, "km"),
         dist_state = units::drop_units(dist_state))

big_cities = cities %>%
  slice_max(population, n = 10)

ggplot() +
  geom_sf(data = south_c)+
  geom_sf(data = cities, aes(col = dist_state), size = .1)+
  geom_sf(data = big_cities, col = "navy") +
  scale_color_gradient(low = "gray", high = "red")+
  ggthemes::theme_map()+
  ggrepel::geom_label_repel(data = big_cities,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 4)
ggplot() +
  geom_sf(data = south_c)+
  geom_sf(data = cities, aes(col = dist_state), size = .1)+
  gghighlight::gghighlight(population > 10000) +
  geom_sf(data = big_cities, col = "navy") +
  scale_color_gradient(low = "gray", high = "red")+
  ggthemes::theme_map()


#3.4
sub_cities = cities %>%
  filter(canmex_diff <= 100) %>%
  slice_max(population, n=5)

ggplot() +
  geom_sf(data = filter(country_b, sov_a3 != "US1")) +
  geom_sf(data = cities, col = "red", alpha = 0.5, size = 0.1) +
  gghighlight::gghighlight(canmex_diff <= 100)+
  geom_sf(data = sub_cities, col = "blue", size = 0.1) +
  ggrepel::geom_label_repel(data = sub_cities,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 2)+
  labs(x = "",
       y = "")
