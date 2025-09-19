#
#
#
#
#
#
#
#
#
#
library(stats19)
library(tidyverse)
library(lubridate)
library(sf)
#
#
#
# Download collisions for 2020 and 2021
collisions_20 = get_stats19(year = 2020, type = "collision")
collisions_21 = get_stats19(year = 2021, type = "collision")
#
#
#
#
library(sf)
# Prep data for plotting map
collisions_sf20 <- collisions_20 |>
    filter(!is.na(location_easting_osgr), !is.na(location_northing_osgr)) |>
    st_as_sf(coords = c("location_easting_osgr", "location_northing_osgr" ))

coords <- st_coordinates(collisions_sf20)

# Round to nearest 100 metres
collisions_sf20 <- collisions_sf20 |>
    mutate(
        x_rounded = round(coords[,1] / 500) * 500,
      y_rounded = round(coords[,2] / 500) * 500)

hotspots20 <- collisions_sf20 |>
  group_by(x_rounded, y_rounded) |>
  summarise(count = n(), .groups = "drop") |>
  st_as_sf(coords = c("x_rounded", "y_rounded")) |>  # no CRS here
  st_set_crs(27700) |>  # assign British National Grid
  st_transform(4326)    # transform to WGS84

#
#
#
ggplot(hotspots20) +
  geom_sf(aes(size = count, color = count, alpha = 0.5)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "UK Road Collision Hotspots (2020)",
    size = "Number of Collisions",
    color = "Number of Collisions"
  )

#
#
#
#
#
library(sf)
# Prep data for plotting map
collisions_sf21 <- collisions_21 |>
    filter(!is.na(location_easting_osgr), !is.na(location_northing_osgr)) |>
    st_as_sf(coords = c("location_easting_osgr", "location_northing_osgr" ))

coords <- st_coordinates(collisions_sf21)

# Round to nearest 100 metres
collisions_sf21 <- collisions_sf21 |>
    mutate(
        x_rounded = round(coords[,1] / 500) * 500,
      y_rounded = round(coords[,2] / 500) * 500)

hotspots21 <- collisions_sf21 |>
  group_by(x_rounded, y_rounded) |>
  summarise(count = n(), .groups = "drop") |>
  st_as_sf(coords = c("x_rounded", "y_rounded")) |>  # no CRS here
  st_set_crs(27700) |>  # assign British National Grid
  st_transform(4326)    # transform to WGS84

#
#
#
ggplot(hotspots21) +
  geom_sf(aes(size = count, color = count, alpha = 0.5)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "UK Road Collision Hotspots (2021)",
    size = "Number of Collisions",
    color = "Number of Collisions"
  )

#
#
#
#
#
hotspots20 <- hotspots20 |> mutate(year = 2020)
hotspots21 <- hotspots21 |> mutate(year = 2021)

hotspots_all <- bind_rows(hotspots20, hotspots21)
#
#
#
ggplot(hotspots_all) +
  geom_sf(aes(size = count, color = count, alpha = 0.5)) +
  scale_color_viridis_c() +
  scale_size(range = c(1, 6)) +
  facet_wrap(~ year) +
  theme_minimal() +
  labs(
    title = "UK Road Collision Hotspots (2020 vs 2021)",
    size = "Number of Collisions",
    color = "Number of Collisions"
  )

#
#
#
#
#
hotspots_filtered <- hotspots_all |>
  filter(count > 10)

ggplot(hotspots_filtered) +
  geom_sf(aes(color = count, alpha = count, size = count)) +
  scale_color_viridis_c() +
  scale_size(range = c(1, 6)) +
  facet_wrap(~ year) +
  theme_minimal() +
  labs(
    title = "UK Collision Hotspots (2020 vs 2021, >10 Collisions in 500m radius)",
    size = "Number of Collisions",
    color = "Number of Collisions"
  )

#
#
#
#
#
#
#
#
#
df_20 <- hotspots20 |>
  st_drop_geometry() |>
  select(x = x_rounded, y = y_rounded, count_2020 = count)

df_21 <- hotspots21 |>
  st_drop_geometry() |>
  select(x = x_rounded, y = y_rounded, count_2021 = count)

 
hotspot_diff = full_join(df_20, df_21, by = c("x", "y")) |>
  mutate(
    count_2020 = replace_na(count_2020, 0),
    count_2021 = replace_na(count_2021, 0),
    diff = count_2021 - count_2020
  ) 

hotspot_diff_sf = hotspot_diff |>
  st_as_sf(coords = c("x", "y"), crs = 27700) |>
  st_transform(4326) |>
  filter(diff < 0  )


ggplot(hotspot_diff_sf) +
  geom_sf(aes(color = diff), size = 2) + 
  scale_color_distiller(palette = "RdYlGn", direction = 1) +
  theme_minimal() +
  labs(
    title = "Change in Collision Hotspots (2021-2020)",
    color = "Change in Collisions"
  )

#
#
#
#
#
#
#
hotspot_negative <- hotspot_diff_sf |>
  filter(diff < 0)

# Plot on an interactive leaflet map
library(leaflet)

leaflet(hotspot_negative) |>
  addProviderTiles("CartoDB.Positron") |>
  addCircleMarkers(
    radius = ~abs(diff) * 0.5,
    color = "red",
    fillOpacity = 0.7,
    stroke = FALSE,
    popup = ~paste0(
      "<b>Change in collisions: </b>", diff,
      "<br><b>2020: </b>", count_2020,
      "<br><b>2021: </b>", count_2021
    )
  ) |>
  addLegend(
    position = "bottomright",
    colors = "red",
    labels = "Decrease in collisions",
    title = "Hotspot Change"
  )
#
#
#
#
#
#
#
#
#
