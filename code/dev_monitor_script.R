

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

data_vp = process_vp_list_rds_1(list_vp_rds)

data_vp_sf = convert_vp_data_to_sf_1(data_vp, 32610)

data_vp %>%
  ggplot() +
  geom_histogram(aes(hms::as_hms(date_time)
                     ,fill = flag_peak_time), bins = 120)  +
  labs(x = "Time of day")

data_vp_sf %>%
  st_drop_geometry() %>%
  select(timestamp, speed_avg, speed_avg_diff) %>%
  pivot_longer(cols = starts_with("speed")) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_grid(rows = vars(name)) +
  scale_y_log10() +
  labs(y = "Count (log10)", x = "Miles per hour")

data_vp %>%
  filter(!is.na(datetime_diff)) %>%
  mutate(datetime_diff_bin = cut(datetime_diff
                                 ,c(0, 5, 10, 15, 20, 25
                                    ,30, 50, 100, 150, 200))
         ,count = 1) %>%
  gauntlet::count_percent_zscore(
    grp_c = c('route_id','datetime_diff_bin')
    ,grp_p = c('route_id',)
    ,col = count
  )

data_vp_pro = make_vp_data_smmry_stats(data_vp)

data_vp_pro %>%
  ggplot(aes(time, total_records, color = as.factor(route_id))) +
  # geom_line() +
  geom_point() +
  # geom_col() +
  scale_x_time(breaks = scales::breaks_width("1 hour")) +
  # facet_grid(row = vars(date), scales = 'free') +
  coord_cartesian(ylim = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(strip.text.y.left = element_text(angle = 0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(y= "", fill = "Rec. Count", x = "Time of Day")


data_vp_pro %>% plot_buses_in_operation()

data_vp %>% plot_query_batch_data(days = 7)












tmp_1 = Sys.time()
tigris::block_groups(48, county = 177)
as.numeric((Sys.time()-tmp_1 ))






















