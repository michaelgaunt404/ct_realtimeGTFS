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

library(tidyverse)
library(gauntlet)
library(here)
library(lubridate)
library(sf)
library(rtgtfsr)
library(mapview)

list_vp_rds = gauntlet::read_rds_allFiles(
  data_location = "data/dev"
  ,specifically = "daily_cache")

data_vp = rtgtfsr::process_vp_list_rds(
  rds_list_objects = list_vp_rds)

data_vp_sf = convert_vp_data_to_sf(data_vp, 32610)

# sample_n(data_vp_sf, 100)
#
# data_vp %>%
#   ggplot() +
#   geom_histogram(aes(hms::as_hms(date_time)
#                      ,fill = flag_peak_time), bins = 120)  +
#   labs(x = "Time of day")
#
# data_vp_sf %>%
#   st_drop_geometry() %>%
#   select(timestamp, speed_avg, speed_avg_diff) %>%
#   pivot_longer(cols = starts_with("speed")) %>%
#   ggplot(aes(value)) +
#   geom_histogram() +
#   facet_grid(rows = vars(name)) +
#   scale_y_log10() +
#   labs(y = "Count (log10)", x = "Miles per hour")
#
# data_vp$query_num  %>% unique()
#
# data_vp %>%
#   filter(!is.na(datetime_diff)) %>%
#   mutate(datetime_diff_bin = cut(datetime_diff
#                                  ,c(0, 5, 10, 15, 20, 25
#                                     ,30, 50, 100, 150, 200))
#          ,count = 1) %>%
#   gauntlet::count_percent_zscore(
#     grp_c = c('route_id','datetime_diff_bin')
#     ,grp_p = c('route_id',)
#     ,col = count
#   )
#
# data_vp_smmry_stats = make_vp_data_smmry_stats(
#   data = data_vp)
#
# data_vp_smmry_stats %>% plot_batch_query_records()
#
# data_vp_smmry_stats %>% plot_buses_in_operation()
#
# data_vp %>% plot_query_batch_data()

##make+speed_profiles==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(gauntlet)
library(here)
library(lubridate)
library(sf)

library(tidytransit)
library(plotly)

#needs this from other script so needs to be saved
# data_vp_sf
# data_vp


#get static GTFS objects
#TODO: need to add the ability to upload many static GTFS files
ct_gtfs = query_and_process_static_gtfs(
  url_gtfs = here::here("data/dev", "ct_gtfs_dl20230601.zip")
  # url_gtfs = "https://www.communitytransit.org/docs/default-source/open-data/gtfs/current.zip"
  )

ct_gtfs_sf = ct_gtfs[["shapes"]]

index_gtsf_shape_xwalk = get_gtfs_shape_xwalk(
  vp_data = data_vp
  ,gtfs = ct_gtfs)

# only here for indivudal inputs for troubleshooting
# over_ride = NA
# over_ride = "202:43:00"
# over_ride = c("202:43:00", "201:104")
# sf_object = ct_gtfs_sf
# crs = 32610
# xwalk_ob = index_gtsf_shape_xwalk
# vp_data = data_vp_sf

list_speed_profiles = make_speed_profiles(
  sf_object = ct_gtfs_sf
  ,crs = 32610
  ,xwalk_ob = index_gtsf_shape_xwalk
  ,vp_data = data_vp_sf
  ,samp_dist = 50
  ,over_ride = c("202:59:00")
)

#select from where to where you want to see data for
temp_map = list_speed_profiles$route_samples %>%
  mapview::mapview(zcol = "index")

#process and filter segements
#NOTE: there will be an issue here if you do not over_ride with one single route
#---you will have to include something that properly filters by shapeid - probs with map
{
index_selected = mapedit::selectFeatures(list_speed_profiles$route_samples) %>%
  st_drop_geometry() %>%
  select(shape_id, index) %>%
  unique()

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

  data_speed_pro_stopped = data_speed_profiles_full %>%
    filter(flag_peak_time != "Untracked") %>%
    filter(current_status == "STOPPED_AT")

  data_speed_pro = data_speed_profiles_full%>%
    filter(flag_peak_time != "Untracked") %>%
    filter(current_status != "STOPPED_AT")
  }


data_speed_pro_stats = speed_profile_index_stats(
  data_speed_pro, grp_c = c('index', 'flag_peak_time', 'direction_id'), quantiles = seq(0, 1, .05)
)



tmp_1 = data_speed_pro_stats %>%
  pivot_longer(cols = c("speed_avg_var", "speed_avg_sd", "speed_avg_stderr", "speed_avg_pre", "rec_count")) %>%
  group_by(flag_peak_time, name) %>%
  mutate(value = zoo::rollmean(value, k = 5, align = "center", na.pad = T) %>%
           dgt2()) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(index, value, color = flag_peak_time), alpha = .6) +
  facet_grid(rows = vars(name), cols = vars(direction_id), scales = "free") +
  labs(x = "Route Segement Number", y = "Metric Value", color = "Week Part") +
  theme(legend.position = "bottom")

plotly::ggplotly(tmp_1)





# plotly::ggplotly(tmp_plot_1)

plot_1 =
  data_speed_pro_stats %>%
  pivot_longer(cols = c("speed_avg_mean", contains("_qt_"))) %>%
  filter(str_detect(name, "mean|.05|.25|.5|.75|.95")) %>%
  group_by(flag_peak_time, name) %>%
  mutate(value = zoo::rollmean(value, k = 5, align = "center", na.pad = T))  %>%
  ggplot() +
  geom_line(aes(index, value, color = name)) +
  facet_grid(rows = vars(flag_peak_time), cols = vars(direction_id), scales = "free") +
  labs(x = "Route Segement Number", y = "Average Segment Speed (mph)", color = "Week Part") +
  theme(legend.position = "bottom")

plot_1 %>% plotly::ggplotly()

data_speed_pro %>%
  ggplot() +
  geom_histogram(aes(ttl_diff)) +
  # geom_histogram(aes(speed_avg)) +
  scale_x_log10()

data_speed_pro %>%
  filter(ttl_diff <= 10) %>%
  # filter(speed_avg <= 2) %>%
  ggplot() +
  geom_bar(aes(index, fill = flag_peak_time), alpha = 1) +
  # geom_point(aes(index, speed_avg, color = flag_peak_time), alpha = .25) +
  # facet_grid(rows = vars(flag_peak_time), cols = vars(direction_id)) +
  labs(x = "Route Segement Number", y = "Average Segment Speed (mph)", color = "Week Part") +
  theme(legend.position = "bottom")

data_speed_pro %>%
  filter(ttl_diff <= 10) %>%
  count(index, direction_id = as.factor(direction_id), samp_lon, samp_lat) %>%
  st_as_sf(coords = c("samp_lon", "samp_lat"), crs = 4326) %>%
  st_jitter(0.0001) %>%
  mapview::mapview(zcol = "direction_id", cex = "n")


##plots about recorded GTFS counts==============================================
data_speed_pro_stopped %>%
  select(trip_id, index) %>%
  unique() %>%
  group_by(index) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_col(aes(index, count))

data_vp_sf %>%
  filter(current_status == "STOPPED_AT") %>%
  select(trip_id) %>%
  mapview::mapview()

data_speed_pro %>%
  count(trip_id, index) %>%
  group_by(index) %>%
  summarise(#sum = sum(n)
            median = median(n)
            ,mean = mean(n)
            ,max = max(n)) %>%
  pivot_longer(cols = !index) %>%
  ggplot() +
  geom_line(aes(index, value, color = name))

#bespoke locations speed and travel time========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



index_selected_2 = mapedit::selectFeatures(list_speed_profiles$route_samples) %>%
  # st_drop_geometry() %>%
  select(shape_id, index) %>%
  unique()

# data_speed_pro %>%
#   filter(direction_id == 0) %>%
#   filter(index >= min(index_selected_2$index) &
#            index <= max(index_selected_2$index)) %>%
#   group_by(trip_id) %>%
#   filter(
#     (index == min(index)) |
#            (index == max(index))) %>%
#   ungroup() %>%
#   count(trip_id)
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






