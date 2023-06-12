

library(tidyverse)
library(gauntlet)
library(rtgtfsr)

tmp_update = here::here("data/data_updates_20230517_171457.rds") %>%
  read_rds()

data_vp %>%
  filter(str_detect(as.character(query_batch), "223602"))

tmp_vp = here::here("data/data_vp_20230517_171457.rds") %>%
  read_rds()

tmp_vp %>%
  select(vehicle.trip.trip_id
         ,vehicle.trip.direction_id
         ,vehicle.trip.route_id
         ,vehicle.trip.start_date
         ,vehicle.vehicle.id) %>%
  unique() %>%
  arrange(vehicle.trip.trip_id) %>%
  group_by(vehicle.trip.trip_id) %>%
  mutate(count_trip_id = n()) %>%
  group_by(vehicle.vehicle.id) %>%
  mutate(count_bus_id = n())



tmp_vp

tmp_update %>% str(max.level = 1)

tmp_update[[1]] %>%  str(max.level = 3)
  .[[2]] %>% str(max.level = 1)
  .[[2]] %>%
  # .$trip  #gives trip charactoristics
  .$stop_time_update %>%
  .[[1]] %>%  flatten_named_list() %>%  pivot_wider()
  # .[[2]]
  str(max.level = 1)


  tmp_update %>%
    # .[1] %>%
    map_df(~{
      info_trip = .x[[1]] %>% .[[2]] %>% .$trip  %>%  data.frame() #gives trip charactoristics .$stop_time_update %>% .[[2]]

      info_timestamp = .x[[1]] %>% .[[2]] %>% .$timestamp %>% data.frame(timestamp = .)

      bind_cols(info_trip, info_timestamp)

      # info_stops = .x[[1]] %>% .[[2]] %>% .$stop_time_update %>%
      #   map_df(~.x %>%  flatten_named_list() %>%  pivot_wider())
      #
      # bind_cols(info_trip, info_timestamp, info_stops) %>%
      #   mutate(time_diff = as.numeric(departure.time) - as.numeric(arrival.time)
      #          ,index = row_number()) %>%
      #   arrange(timestamp, stop_sequence)

    }) %>%
    unique()
  tmp_update[[1]] %>%  .[[1]] %>% .[[2]] %>% .$trip %>% data.frame()
