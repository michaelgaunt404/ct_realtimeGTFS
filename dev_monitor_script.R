

install.packages('skimr')
install.packages('sf')
library(tidyverse)
library(magrittr)
library(here)
library(sf)
library(mapview)
install.packages("crosstalk")


list_vp_rds = gauntlet::read_rds_allFiles(
  specifically = "data_vp")

data_vp = process_vp_list_rds(list_vp_rds)

data_vp_sf = convert_vp_data_to_sf(data_vp, 32610)

data_vp_sf %>%
  filter(current_status == "IN_TRANSIT_TO") %>%
mapview::mapview(zcol = "speed_avg")

data_vp %>%
  filter(!is.na(datetime_diff)) %>%
  mutate(datetime_diff_bin = cut(datetime_diff, c(0, 5, 10, 15, 20, 25, 30, 50, 100, 150, 200))
         ,count = 1) %>%
  gauntlet::count_percent_zscore(
    grp_c = c('route_id','datetime_diff_bin')
    ,grp_p = c('route_id',)
    ,col = count
  )


data_vp %>%
  count(query_batch, route_id, vehicle_id) %>%
  group_by(query_batch, route_id) %>%
  summarise(buses_in_service= n()
            ,total_records = sum(n)) %>%
  ungroup() %>%
  mutate(time = hms::as_hms(floor_date(query_batch, "minute"))
        ,date = date(query_batch)) %>%
  ggplot(aes(time, total_records, color = as.factor(route_id))) +
  geom_line() +
  geom_point() +
  scale_x_time(breaks = scales::breaks_width("1 hour")) +
  facet_grid(row = vars(date), scales = 'free') +
  coord_cartesian(ylim = c(0, NA))

data_vp %>%
  count(time = hms::as_hms(floor_date(date_time, "minute"))
        ,date = date(date_time)) %>%
  ggplot() +
  geom_tile(aes(time, y = 1,  fill = n)) +
  facet_grid(row = vars(date)) +
  scale_x_time(breaks = scales::breaks_width("1 hour"))






















tmp %>%  names()

item = temp_data %>%
  # filter(str_detect(id, "_15109")) %>%
  # filter(vehicle.timestamp == 1683572554) %>%
  filter(vehicle.trip.route_id == 201 |
           vehicle.trip.route_id == 202) %>%
  filter(vehicle.current_status != "IN_TRANSIT_TO") %>%
  arrange(vehicle.trip.route_id, vehicle.vehicle.id, id) %>%
  group_by(vehicle.trip.route_id, vehicle.vehicle.id, vehicle.trip.trip_id) %>%
  mutate(date_time = #gsub('_.*', "\\1", 1683572554)
           vehicle.timestamp %>%
           as.numeric() %>%
           lubridate::as_datetime() %>%
           lubridate::with_tz("US/Pacific")) %>%
  mutate(datetime_diff = as.numeric(date_time-lag(date_time))) %>%
  ungroup() %>%
  data.frame() %>%
  filter(datetime_diff != 0) %>%
  sf::st_as_sf(coords = c('vehicle.position.longitude', 'vehicle.position.latitude'), crs = 4326) %>%
  sf::st_transform(32610) %>%
  gauntlet::st_extract_coords() %>%
  group_by(vehicle.trip.route_id, vehicle.vehicle.id, vehicle.trip.trip_id) %>%
  mutate(lon_diff = lon-lag(lon)
         ,lat_diff = lat-lag(lat)
         ,ttl_diff = sqrt(lon_diff**2 + lat_diff**2)
         ,speed_avg = (ttl_diff/datetime_diff)*2.236936) %>%
  ungroup() %>%
  sf::st_transform(4326) %>%
  st_jitter() %>%
  mapview::mapview(#zcol = "vehicle.timestamp"
    zcol = "speed_avg")








data_vp %>%
  names()

data_vp %>%
  map(nrow)









