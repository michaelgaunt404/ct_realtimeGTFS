

library(tidyverse)
library(magrittr)
library(here)
library(sf)
library(mapview)
library(gauntlet)
library(rtgtfsr)


list_vp_rds = gauntlet::read_rds_allFiles(
  data_location = "data"
  ,specifically = "data_vp")

data_vp = process_vp_list_rds(list_vp_rds)

data_vp = data_vp %>%
  arrange(query_batch, route_id, vehicle_id, trip_id) %>%
  group_by(route_id, vehicle_id, trip_id) %>%
  mutate(date_time = timestamp)

data_vp_sf = convert_vp_data_to_sf(data_vp, 32610) %>%
  group_by(route_id, vehicle_id, trip_id) %>%
  mutate(speed_avg_diff = speed_avg-lag(speed_avg))

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
  scale_x_time(breaks = scales::breaks_width("1 hour")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
































