
library(tidyverse)
library(tidytransit)
library(plotly)


ct_gtfs = query_and_process_gtfs(
  gtfs_path = here::here("data", "ct_gtfs/current.zip"))

ct_gtfs_sf = ct_gtfs[["shapes"]]

index_gtsf_shape_xwalk = get_gtsf_shape_xwalk(data_vp, ct_gtfs)



over_ride = "202:43:00"
over_ride = c("202:43:00", "201:104")


sf_object = ct_gtfs_sf
crs = 32610
xwalk_ob = index_gtsf_shape_xwalk
vp_data = data_vp_sf


list_speed_profiles = make_speed_profiles(
  sf_object = ct_gtfs_sf
  ,crs = 32610
  ,xwalk_ob = index_gtsf_shape_xwalk
  ,vp_data = data_vp_sf
  ,samp_dist = 10
  ,over_ride = c("202:59:00")
)


temp_plot = list_speed_profiles$speed_profiles %>%
  filter(index >= 2100 &
           index < 3500) %>%
  filter(direction_id == 1) %>%
  filter(flag_peak_time != "Untracked") %>%
  filter(current_status != "STOPPED_AT") %>%
  ggplot() +
  geom_point(aes(index, speed_avg, color = flag_peak_time), alpha = .25) +
  facet_grid(rows = vars(flag_peak_time), cols = vars(direction_id)) +
  coord_cartesian(ylim = c(0, 55))

temp_map = list_speed_profiles$route_samples %>%
  # filter(index >= 2100 &
  #          index < 2700) %>%
  mapview(zcol = "index")



library(crosstalk)

bscols(widths = c(8, 4)
       ,plotly::ggplotly(temp_plot)
       ,temp_map@map)













































install.packages("dbscan")

library(dbscan)
dbscan::dbscan()

tmppp = dat_speed_profiles %>%
  filter(index >= 2100 &
           index < 2700) %>%
  filter(flag_peak_time != "Untracked") %>%
  filter(current_status != "STOPPED_AT") %>%
  filter(speed_avg <= 50) %>%
  mutate(seconds = date_time %>%  hms::as_hms() %>%  seconds()) %>%
  select(flag_peak_time, direction_id, index, speed_avg, speed_avg_diff, seconds)

tmppp = tmppp %>%
  group_by(flag_peak_time, direction_id, index) %>%
  nest() %>%
  mutate(
    mean = map(data, ~weighted.mean(.x$time_qfree, .x$count, na.rm = T)),
    qauntiles = map(data, ~group_wtd_quantiles(.x, value = "time_qfree", quantiles = c(.5, .95), weight = "count"))) %>%
  unnest(cols = c(mean, qauntiles))

tmppp %>%
  mutate(count = 1) %>%
  group_by(flag_peak_time, direction_id, index) %>%
  nest() %>%
  mutate(
    qauntiles = map(data, ~group_wtd_quantiles(.x, value = "speed", quantiles = c(.5, .95), weight = "count")))
  mutate(ts_mean = mean(speed_avg)
         ,ts_median = median(speed_avg)
         ,ts_var = var(speed_avg)
         ,ts_max = max(speed_avg)
         ,ts_min = min(speed_avg)) %>%
  ungroup()  %>%
  na.omit()



gauntlet::group_wtd_quantiles()

object =
  tmppp %>%
  # select(index, speed_avg, speed_avg_diff) %>%
  select(!c(flag_peak_time, direction_id)) %>%
  mutate(across(everything(), scale)) %>%
  hdbscan(minPts = 15)

plot(object, show_flat = F)

tmppp %>%
  mutate(cluster = as.factor(object$cluster) ) %>%
  ggplot() +
  geom_point(aes(index, cluster), alpha = .25) +
  facet_grid(rows = vars(flag_peak_time), cols = vars(direction_id))


tmppp %>%
  mutate(cluster = as.factor(object$cluster) ) %>%
  ggplot() +
  geom_point(aes(index, speed_avg, color = cluster), alpha = .25) +
  facet_grid(rows = vars(cluster))
  facet_grid(rows = vars(flag_peak_time), cols = vars(direction_id)) +
  coord_cartesian(ylim = c(0, 55))


lubridate::



library(dplyr)

tmp = st_join(x = data_vp_sf
        ,y = sf_object_points
        ,join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  view()


tmp_pro = tmp %>%
  filter(index >= 2100 &
           index < 2700) %>%
  # filter(current_status == "IN_TRANSIT_TO") %>%
  # mutate(flag_peak_time = case_when(
  #   hour(date_time)>=6&hour(date_time)<9~"AM Peak"
  #   ,hour(date_time)>=16&hour(date_time)<18~"PM Peak"
  #   ,hour(date_time)>=10&hour(date_time)<14~"Midday"
  #   ,T~"Untracked")) %>%
  filter(!is.na(speed_avg_diff)) %>%
  filter(flag_peak_time != "Untracked") %>%
  group_by(flag_peak_time, direction_id, index) %>%
  mutate(ts_mean = mean(speed_avg)
         ,ts_median = median(speed_avg)
         ,ts_var = var(speed_avg)
         ,ts_count = n()) %>%
  ungroup() %>%
  arrange(index) %>%
  group_by(flag_peak_time, direction_id)  %>%
  mutate(auto_make_rMean_col(ts_median, width = c(3, 5, 7)))

tmp_plot = tmp_pro %>%
  ungroup() %>%
  select(index, direction_id, flag_peak_time, speed_avg, speed_avg_diff, ts_median_rMean_7, ts_count) %>%
  pivot_longer(cols = c(speed_avg, speed_avg_diff, ts_median_rMean_7, ts_count)) %>%
  filter(str_detect(flag_peak_time, "AM Peak")) %>%
  ggplot() +
  geom_point(aes(index, value), alpha = .25) +
  facet_grid(rows = vars(name)
             ,cols = vars(direction_id, flag_peak_time)
             ,scales = "free") +
  scale_color_viridis_c()

plotly::ggplotly(tmp_plot)


tmp_plot = tmp_pro %>%
  ggplot() +
  geom_point(aes(index, speed_avg, color = speed_avg), alpha = .25) +
  geom_line(aes(index, ts_count)) +
  facet_grid(rows = vars(flag_peak_time)
             ,cols = vars(direction_id)) +
  scale_color_viridis_c()

tmpppp = plotly::ggplotly(tmp_plot)

yolo = sf_object_points %>%
  filter(index >= 2100 &
           index < 2700) %>%
  mapview(zcol = "index")

library(crosstalk)
bscols(widths = c(12, 12)
       ,tmpppp
       ,yolo@map)

mapview::mapview(sf_object_points, zcol = "index")





