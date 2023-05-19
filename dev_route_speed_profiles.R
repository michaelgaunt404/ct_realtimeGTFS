
library(tidyverse)
library(tidytransit)
library(plotly)


nyc <- read_gtfs(local_gtfs_path)


# nyc <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")

local_gtfs_path <- system.file("extdata",
                               "google_transit_nyc_subway.zip",
                               package = "tidytransit")
nyc = read_gtfs(local_gtfs_path)




https://www.communitytransit.org/docs/default-source/open-data/gtfs/current.zip?sfvrsn=988306d7_20


url_gtfs = "https://www.communitytransit.org/docs/default-source/open-data/gtfs/current.zip"

ct_gtfs = tidytransit::read_gtfs(
  url_gtfs
)

ct_gtfs_sf = gtfs_as_sf(ct_gtfs)
# ct_routes = ct_gtfs_sf$routes

ct_routes = ct_gtfs_sf$shapes

tmp = ct_routes %>%
  filter(str_detect(shape_id, "202:59"))



tmp %>%
  sf::lin



sf_object = tmp
crs = 32610

temp = sf_object %>%
  mutate(merge_id = row_number())

sf_object_linestring = temp %>%
  st_transform(crs) %>%
  st_cast("LINESTRING") %>%
  mutate(linestring_id = row_number()) %>%
  select(merge_id, linestring_id)

sf_object_points = sf_object_linestring %>%
  st_line_sample(density = 1/10) %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  st_cast("POINT") %>%
  mutate(index = row_number())

(sf_object_linestring %>%
    st_line_sample(density = 1/10) %>%
    st_transform(4326) %>%
    mapview::mapview()) + mapview::mapview(data_vp_sf, zcol = "speed_avg")

sf_object_points %>%
  mapview(index = "zcol")

library(dplyr)

tmp = st_join(x = data_vp_sf %>%
          filter(direction_id == 1)
        ,y = sf_object_points
        ,join = st_nearest_feature) %>%
  st_drop_geometry()

tmp_polt = tmp %>%
  filter(current_status == "IN_TRANSIT_TO") %>%
  mutate(flag_peak_time = case_when(
    hour(date_time)>=6&hour(date_time)<9~"AM Peak"
    ,hour(date_time)>=16&hour(date_time)<18~"PM Peak"
    ,hour(date_time)>=10&hour(date_time)<14~"Midday"
    ,T~"Untracked")) %>%
  filter(!is.na(speed_avg_diff)) %>%
  # group_by(flag_peak_time, index) %>%
  # summarise(speed_avg = mean(speed_avg)) %>%
  ungroup() %>%
  ggplot(aes(index, speed_avg_diff, color = speed_avg_diff)) +
  geom_point(alpha = .25) +
  # geom_line() +
  facet_grid(rows = vars(flag_peak_time)) +
  scale_color_viridis_c()

plotly::ggplotly(tmp_polt)

mapview::mapview(sf_object_points, zcol = "index")


data_vp_sf %>%
  filter(direction_id == 1) %>%
  mapview()















