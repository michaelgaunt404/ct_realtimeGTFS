#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)
library(here)
library(lubridate)
library(sf)
library(rtgtfsr)
library(mapview)
library(tidytransit)
library(plotly)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##monitor script==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#process_data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#realtime_gtfs_data=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#this reads in all your data, i beleive the daily_chaches are saved as RDS to save space
#this should be its own target - list of RDS objects before processing
list_vp_rds = gauntlet::read_rds_allFiles(
  data_location = "data/daily_cache_vp"
  ,specifically = "daily_cache")

#might be another good candidate for a target
#------directions might not always be "north" and "south"
data_vp = rtgtfsr::process_vp_list_rds(
  rds_list_objects = list_vp_rds) %>%
  mutate(direction_id = case_when(direction_id == 1~"north_bound"
                                  ,direction_id == 0~"south_bound"
                                  ,T~"problem"))

#probably another good target - calculates speed given simple euclidean dis
data_vp_sf = convert_vp_data_to_sf(data_vp, 32610)

#static_gtfs_data=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ct_gtfs = query_and_process_static_gtfs(
  url_gtfs = here::here("data", "ct_gtfs_dl20230601.zip"))

index_gtsf_shape_xwalk = get_gtfs_shape_xwalk(
  vp_data = data_vp
  ,gtfs = ct_gtfs)

ct_gtfs_sf = ct_gtfs[["shapes"]]

list_speed_profiles = make_speed_profiles(
  sf_object = ct_gtfs_sf
  ,crs = 32610
  ,xwalk_ob = index_gtsf_shape_xwalk
  ,vp_data = data_vp_sf
  ,samp_dist = 50
  ,over_ride = c("202:59:00")
)

#this has opportunity to be selected, would probably want to make it robust
#-------select different start, stop points for different shape_ids
index_selected = list_speed_profiles$route_samples %>%
  filter(index %in% c(419, 712)) %>%
  select(shape_id, index) %>%
  st_drop_geometry()

#this should also be a function
{
week_core = c("Tue", "Wed", "Thu")
week_end = c("Sat", "Sun")

data_speed_profiles_full =
  list_speed_profiles$speed_profiles %>%
  filter(index >= min(index_selected$index) &
           index <= max(index_selected$index)) %>%
  mutate(day_of_week = wday(query_batch, label = T)) %>%
  mutate(flag_week_part = case_when(
    day_of_week %in% week_core~"week_core"
    ,day_of_week %in% week_end~"week_end"
    ,T~"week_untracked"
  ))
  }

data_speed_pro_stopped = data_speed_profiles_full %>%
  filter(flag_peak_time != "Untracked") %>%
  filter(current_status == "STOPPED_AT")

data_speed_pro = data_speed_profiles_full%>%
  filter(flag_peak_time != "Untracked") %>%
  filter(current_status != "STOPPED_AT")

data_speed_pro_stats = speed_profile_index_stats(
  data_speed_pro
  ,grp_c = c('index', 'flag_peak_time', 'direction_id')
  ,quantiles = seq(0, 1, .05)
)





#
# data_speed_pro %>%
#   ggplot() +
#   geom_histogram(aes(ttl_diff)) +
#   # geom_histogram(aes(speed_avg)) +
#   scale_x_log10()
#
# data_speed_pro %>%
#   filter(ttl_diff <= 10) %>%
#   # filter(speed_avg <= 2) %>%
#   ggplot() +
#   geom_bar(aes(index, fill = flag_peak_time), alpha = 1) +
#   # geom_point(aes(index, speed_avg, color = flag_peak_time), alpha = .25) +
#   # facet_grid(rows = vars(flag_peak_time), cols = vars(direction_id)) +
#   labs(x = "Route Segement Number", y = "Average Segment Speed (mph)", color = "Week Part") +
#   theme(legend.position = "bottom")
#
# data_speed_pro %>%
#   filter(ttl_diff <= 10) %>%
#   count(index, direction_id = as.factor(direction_id), samp_lon, samp_lat) %>%
#   st_as_sf(coords = c("samp_lon", "samp_lat"), crs = 4326) %>%
#   st_jitter(0.0001) %>%
#   mapview::mapview(zcol = "direction_id", cex = "n")


# ##plots about recorded GTFS counts==============================================
# data_speed_pro_stopped %>%
#   select(trip_id, index) %>%
#   unique() %>%
#   group_by(index) %>%
#   summarise(count = n()) %>%
#   ggplot() +
#   geom_col(aes(index, count))
#
# data_vp_sf %>%
#   filter(current_status == "STOPPED_AT") %>%
#   select(trip_id) %>%
#   mapview::mapview()
#
# data_speed_pro %>%
#   count(trip_id, index) %>%
#   group_by(index) %>%
#   summarise(#sum = sum(n)
#             median = median(n)
#             ,mean = mean(n)
#             ,max = max(n)) %>%
#   pivot_longer(cols = !index) %>%
#   ggplot() +
#   geom_line(aes(index, value, color = name))

#bespoke locations speed and travel time========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



index_selected_3 = mapedit::selectFeatures(list_speed_profiles$route_samples) %>%
  # st_drop_geometry() %>%
  select(shape_id, index) %>%
  unique()

# data_speed_pro %>%
#   filter(direction_id == "south_bound") %>%
#   filter(index >= min(index_selected_2$index) &
#            index <= max(index_selected_2$index)) %>%
#   group_by(trip_id) %>%
#   filter(
#     (index == min(index)) |
#            (index == max(index))) %>%
#   ungroup() %>%
#   count(trip_id)

tmp = data_vp_sf %>%
  filter(trip_id == "11043502__MCOB-DO:121:0:Weekday:2:23MAR:41006:12345")

index_trip_id = data_vp_sf$trip_id %>% sample(5)
crs_to = 32610

yolo_3 = data_vp_sf %>%
  filter(current_status == "IN_TRANSIT_TO") %>%
  group_by(
    trip_id_1 = trip_id
    ,vehicle_id_1 = vehicle_id
    ,direction_id_1 = direction_id) %>%
  group_map(~{
    index_close = st_nearest_feature(index_selected_3, .x, pairwise = T)

  .x[index_close, ] %>%
      arrange(date_time) %>%
      sf::st_transform(crs = crs_to) %>%
      gauntlet::st_extract_coords() %>%
      mutate(time_diff = as.numeric(date_time - lag(date_time))*60) %>%
      mutate(
        lon_diff = lon - lag(lon),
        lat_diff = lat - lag(lat),
        ttl_diff = sqrt(lon_diff^2 + lat_diff^2),
        speed_avg = (ttl_diff / time_diff) * 2.236936
      ) %>%
      st_transform(4326)

  }) %>%
  reduce(bind_rows)

index_selected_2 %>%
  quick_buffer(rad = 200) %>%
  mapview() +
  mapview(yolo)

inflated_2 = index_selected_2 %>% quick_buffer(rad = 500)
inflated_3 = index_selected_3 %>% quick_buffer(rad = 500)

min_distance_2 = as.numeric(st_distance(inflated_2[2, ], inflated_2[1, ])[[1]])
min_distance_3 = as.numeric(st_distance(inflated_3[2, ], inflated_3[1, ])[[1]])


yolo_2_pro = yolo %>%
  st_filter(inflated_2)  %>%
  st_drop_geometry() %>%
  select(trip_id, direction_id, route_id
         ,date_time, time_diff, ttl_diff, vehicle_id) %>%
  group_by(trip_id, direction_id, route_id, vehicle_id) %>%
  # mutate(count = n()) %>%
  # ungroup()
  filter(n() == 2) %>%
  ungroup() %>%
  filter(!is.na(ttl_diff)) %>%
  filter(ttl_diff > min_distance_2)


yolo_3_pro = yolo_3 %>%
  st_filter(inflated_3)  %>%
  st_drop_geometry() %>%
  select(trip_id, direction_id, route_id
         ,date_time, time_diff, ttl_diff, vehicle_id) %>%
  group_by(trip_id, direction_id, route_id, vehicle_id) %>%
  # mutate(count = n()) %>%
  # ungroup()
  filter(n() == 2) %>%
  ungroup() %>%
  filter(!is.na(ttl_diff)) %>%
  filter(ttl_diff > min_distance_3)

yolo_2_pro %>%
  # select(id, direction_id, route_id, date_time, time_diff, ttl_diff) %>%
  plotly::plot_ly(data = ., x = ~time_diff/60/60, y = ~ttl_diff/5280
                  ,color = ~hour(date_time)
                  ,type = "scatter")



data_comb = yolo_2_pro %>%
    merge(
      yolo_3_pro %>% rename_with(~paste0(.x, "_full"))
      ,by.x = c("trip_id", "direction_id", "route_id", "vehicle_id")
      ,by.y = c("trip_id_full", "direction_id_full", "route_id_full", "vehicle_id_full")
    )

data_comb %>%
  filter(direction_id == "north_bound") %>%
  ggplot() +
  geom_point(aes(time_diff, time_diff_full, color = as.factor(hour(date_time_full)))) +
  geom_smooth(aes(time_diff, time_diff_full), method = "lm")


lm(time_diff_full~time_diff*as.factor(hour(date_time_full))
   , data = data_comb %>%
    filter(direction_id != "north_bound")
   ) %>% summary()

3.6065


data_weird = yolo %>% filter(ttl_diff/5280 > 3)

index_trip_weird_2 = data_weird$trip_id %>% unique() %>%  sample(1)

item = yolo %>%
  filter(trip_id %in% c(index_trip_weird_2)) %>%
  select( "trip_id", "direction_id", "route_id", "vehicle_id", "date_time"
          ,"time_diff", "ttl_diff") %>%
  filter(ttl_diff/5280 > 3)

data_vp_sf %>%
  filter(str_detect(trip_id, item$trip_id)
         ,direction_id == item$direction_id
         ,route_id == item$route_id
         ,vehicle_id == item$vehicle_id) %>%
  mapview(zcol = "timestamp")







  ggplot() +
  # geom_density(aes(speed_avg))
  geom_point(aes(time_diff, ttl_diff, color = speed_avg))
  # geom_jitter(aes(time_diff, ttl_diff))


index = st_nearest_feature(index_selected_2[1, ], tmp, pairwise = T)

tmpppp = tmppp[index, ] %>%
  select(direction_id, date_time )

index_selected_2 %>%
    mapview() +
    mapview(tmp) +
    mapview(tmpppp)

  tmpppp %>%
    arrange(date_time) %>%
    select(id, trip_id, direction_id, route_id, date_time) %>%
    mutate(time_diff = seconds(date_time - lag(date_time))) %>%
    filter(!is.na(time_diff))
#
# result =
#   data_speed_pro %>%
#   filter(direction_id == 1) %>%
#     filter(index >= min(index_selected_2$index) &
#              index <= max(index_selected_2$index)) %>%
#   group_by(trip_id) %>%
#   filter(index == min(index) | index == max(index)) %>%
#   filter((trip_id == trip_id[which.min(index)] & date_time == min(date_time)) |
#            (trip_id == trip_id[which.max(index)] & date_time == max(date_time))) %>%
#   ungroup() %>%
#   arrange(trip_id) %>%
#     # head(2) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 32610)
#
#
# result %>%
#   st_drop_geometry() %>%
#   arrange(trip_id, start_date, vehicle_id, date_time) %>%
#   group_by(trip_id, start_date, vehicle_id) %>%
#   mutate(datetime_diff_1 = crrct0(date_time) %>%
#            as.numeric()/60
#          ,datetime_diff_2 = date_time - lag(date_time)) %>%
#   ungroup() %>% clipr::write_clip()
# filter(datetime_diff > 1) %>%
#   # select(datetime_diff)
#   ggplot() +
#   geom_histogram(aes(datetime_diff))
# glimpse()
#
#
# data_speed_pro %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 32610) %>%
#   st_transform(4326) %>%
#   st_join(index_selected_2[1,], join = st_nearest_feature)
#
# tmp = data_speed_pro %>%
#   group_by(trip_id, start_date, vehicle_id) %>%
#   group_map(~{
#     index_selected_2[1,] %>%
#         st_join(.x %>%
#                   st_as_sf(coords = c("lon", "lat"), crs = 32610) %>%
#                   st_transform(4326), join = st_nearest_feature)
#     }) %>%
#     reduce(bind_rows)
#
#  tmp_1 = data_speed_pro %>%
#    group_by(trip_id, start_date, vehicle_id) %>%
#    group_map(~{
#      index_selected_2[2,] %>%
#        st_join(.x %>%
#                  mutate(lon_1 = lon, lat_1 = lat) %>%
#                  st_as_sf(coords = c("lon_1", "lat_1"), crs = 32610) %>%
#                  st_transform(4326), join = st_nearest_feature)
#    }) %>%
#    reduce(bind_rows)


 ##better_method================================================================

 # tmp = c(1, 2) %>%
 #   map_df(~{
 #     index = .x
 #
 #     data_speed_pro %>%
 #       group_by(trip_id, start_date, vehicle_id) %>%
 #       group_map(~{
 #         tmp = st_nearest_feature(
 #           index_selected_2
 #           ,.x %>%
 #             mutate(lon_1 = lon, lat_1 = lat) %>%
 #             st_as_sf(coords = c("lon_1", "lat_1"), crs = 32610) %>%
 #             st_transform(4326))
 #
 #         .x[tmp,]
 #       }) %>%
 #       reduce(bind_rows)
 #   })

 ##better_method================================================================
 #doing something here to test the process
# filter_temp =  data_speed_pro %>%
#    select(trip_id, start_date, vehicle_id) %>%
#    unique() %>%
#    head(1)
#
# tmp = filter_temp %>%
#   head()%>%
#   group_by(trip_id, start_date, vehicle_id) %>%
#   group_map(~{
#     tmp = st_nearest_feature(
#       index_selected_2
#       ,.x %>%
#         mutate(lon_1 = lon, lat_1 = lat) %>%
#         st_as_sf(coords = c("lon_1", "lat_1"), crs = 32610) %>%
#         st_transform(4326))
#
#     .x[tmp,]
#   }) %>%
#   reduce(bind_rows)
#
#  tmp$ %>%
#    st_as_sf(coords = c("lon", "lat"), crs = 32610) %>%
#    st_transform(4236) %>%
#    mapview()
#
#  tmp %>%
#    count(trip_id, start_date, vehicle_id)
#
# st_nearest_feature(index_selected_2, )
#
#  index_selected_2

 filter_temp =  data_speed_pro %>%
   select(trip_id, start_date, vehicle_id) %>%
   unique() %>%
   sample_n(50)

 # list(
 #   filter_temp$trip_id
 #   ,filter_temp$start_date
 #   ,filter_temp$vehicle_id) %>%
 # pmap(function(x, y, z) {
 #   # tmp = st_nearest_feature(
 #     # index_selected_2
 #     # ,
 #     yolo <<- data_speed_pro %>%
 #       filter(trip_id == x
 #              ,start_date == y
 #              ,vehicle_id == z) %>%
 #     # select(lon, lat) %>% print()
 #       mutate(lon_1 = lon, lat_1 = lat) %>%
 #       st_as_sf(coords = c("lon_1", "lat_1"), crs = 32610) %>%
 #       st_transform(4326) %>%
 #     mapview() + mapview(index_selected_2, col.regions = "red")
 #   # )
 #
 # })


temp = list(
  filter_temp$trip_id
  ,filter_temp$start_date
  ,filter_temp$vehicle_id) %>%
  pmap_df(function(x, y, z) {
    tmp_data = data_speed_pro %>%
      filter(trip_id == x
             ,start_date == y
             ,vehicle_id == z)

    tmp_data_sf = tmp_data %>%
      mutate(lon_1 = lon, lat_1 = lat) %>%
      st_as_sf(coords = c("lon_1", "lat_1"), crs = 32610) %>%
      st_transform(4326)

    index_nnbr = st_nearest_feature(
      index_selected_2
      ,tmp_data_sf
    )

    check_same_points = (index_nnbr[[1]] != index_nnbr[[2]])

    print(check_same_points)

    if (check_same_points) {
      merged = tmp_data[index_nnbr,] %>%
        bind_cols(index_selected_2 %>%
                    gauntlet::st_extract_coords() %>%
                    rename(lat_deg = lat, lon_deg = lon) %>%
                    st_transform(32610) %>%
                    gauntlet::st_extract_coords() %>%
                    rename(lat_m = lat, lon_m = lon) %>%
                    st_drop_geometry()) %>%
        arrange(date_time) %>%
        mutate(
          lon_diff_m = lon - lon_m,
          lat_diff_m = lat - lat_m,
          ttl_diff_m = sqrt(lon_diff_m^2 + lat_diff_m^2),
          datetime_diff_m = as.numeric(date_time-lag(date_time)),
          index_row = as.factor(row_number())
        )
    } else {
      merged = data.frame()

    }

  })


tmp_map = temp %>%
  mutate(lon_1 = lon, lat_1 = lat) %>%
  st_as_sf(coords = c("lon_1", "lat_1"), crs = 32610) %>%
  st_transform(4326) %>%
  mapview(zcol = "index_row")

tmp_map@map = tmp_map@map %>%
  leaflet::addMeasure(primaryLengthUnit = "meter")

tmp_map + mapview(index_selected_2, col.regions = "red")

temp_pro = temp %>%
  filter(!is.na(datetime_diff_m))

boot::boot(temp_pro$datetime_diff_m, statistic = mean, R = 5000) %>%
  print()

temp_pro %>%
  ggplot() +
  geom_boxplot(aes(datetime_diff_m, color = flag_peak_time)) +
  facet_grid(rows = vars(direction_id))






