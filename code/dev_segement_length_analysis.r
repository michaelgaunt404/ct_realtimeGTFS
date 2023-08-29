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
library(googledrive)

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
files = googledrive::drive_find("cache_vp")

files %>%
  split(., rownames(.)) %>%
  map(~{
    gauntlet::gdrive_files_to_rds(
      data_to_fetch = .x
      ,save_format = ".rds"
      ,save_location = "data/daily_cache_dl_20230828"
    )
  })


#realtime_gtfs_data=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#this reads in all your data, i beleive the daily_chaches are saved as RDS to save space
#this should be its own target - list of RDS objects before processing
list_vp_rds = gauntlet::read_rds_allFiles(
  data_location = "data/daily_cache_dl_20230828"
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





#bespoke locations speed and travel time========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



index_selected_4 = mapedit::selectFeatures(list_speed_profiles$route_samples) %>%
  # st_drop_geometry() %>%
  select(shape_id, index) %>%
  unique() %>%
  arrange(index)

index_selected_4_ends = mapedit::selectFeatures(index_selected_4) %>%
  select(shape_id, index) %>%
  mutate(flag_ends = "end") %>%
  unique() %>%
  arrange(index)

combined = index_selected_4 %>%
  merge(index_selected_4_ends %>% st_drop_geometry(), all = T) %>%
  mutate(flag_ends = replace_na(flag_ends, "middle")
         ,flag_ends_lag = lag(flag_ends)%>%
           replace_na("end")
         ,index_lag = lag(index)%>%
           replace_na(max(index_selected_4_ends$index))) %>%
  mutate(name = c("Hewitt", "14th", "100th")) %>%
  # mutate(name = c("Hewitt", "23rd", "18th", "14th", "ramp"
  #                 ,"4th", "grove", "88th", "100th")) %>%
  mutate(name_lag = lag(name)%>%
           replace_na("100th"))

saveRDS(combined, here::here("data/combined.rds"))
combined = read_rds(here::here("data/combined.rds"))


library(furrr)

future::multisession(workers = 6)

tictoc::tic()

crs_to = 32610

# temp_output = combined %>%
#   select(index, index_lag, name, name_lag, flag_ends, flag_ends_lag) %>%
#   st_drop_geometry() %>%
#   split(., rownames(.)) %>%
#   future_map(~{
#     message(str_glue("Processing {paste0(.x[c(1, 2)], collapse = '_')}...."))
#     index_segement = combined %>%
#       filter(index %in% .x)
#
#     temp_processed = data_vp_sf %>%
#       filter(current_status == "IN_TRANSIT_TO") %>%
#       group_by(
#         trip_id_1 = trip_id
#         ,vehicle_id_1 = vehicle_id
#         ,direction_id_1 = direction_id) %>%
#       group_map(~{
#         index_close = st_nearest_feature(index_segement, .x, pairwise = T)
#
#         .x[index_close, ] %>%
#           arrange(date_time) %>%
#           sf::st_transform(crs = crs_to) %>%
#           gauntlet::st_extract_coords() %>%
#           mutate(time_diff = as.numeric(date_time - lag(date_time))*60) %>%
#           mutate(
#             lon_diff = lon - lag(lon),
#             lat_diff = lat - lag(lat),
#             ttl_diff = sqrt(lon_diff^2 + lat_diff^2),
#             speed_avg = (ttl_diff / time_diff) * 2.236936
#           ) %>%
#           st_transform(4326)
#       }) %>%
#       reduce(bind_rows) %>%
#       mutate(segement = paste0(.x[c(1, 2)], collapse = "_")
#              ,segement_name = paste0(.x[c(3, 4)], collapse = "_")
#              ,segement_type = paste0(.x[c(5, 6)], collapse = "_"))
#
#     index_segement_inflated = index_segement %>% quick_buffer(rad = 500)
#
#     min_distance = as.numeric(
#       st_distance(index_segement_inflated[2, ]
#                   ,index_segement_inflated[1, ])[[1]])
#
#     temp_processed %>%
#       st_filter(index_segement_inflated)  %>%
#       st_drop_geometry() %>%
#       select(trip_id, direction_id, route_id, date_time, vehicle_id, segement
#              ,segement_name, segement_type, time_diff, ttl_diff, speed_avg) %>%
#       group_by(trip_id, direction_id, route_id
#                ,vehicle_id, segement, segement_name, segement_type) %>%
#       filter(n() == 2) %>%
#       ungroup() %>%
#       filter(!is.na(ttl_diff)) %>%
#       filter(ttl_diff > min_distance)
#   }, .progress = T)





temp_output_halfMeth = combined %>%
  select(index, index_lag, name, name_lag, flag_ends, flag_ends_lag) %>%
  st_drop_geometry() %>%
  split(., rownames(.)) %>%
  future_map(~{
    message(str_glue("Processing {paste0(.x[c(1, 2)], collapse = '_')}...."))
    index_segement = combined %>%
      filter(index %in% .x)

    temp_processed = data_vp_sf %>%
      filter(current_status == "IN_TRANSIT_TO") %>%
      group_by(
        trip_id_1 = trip_id
        ,vehicle_id_1 = vehicle_id
        ,direction_id_1 = direction_id) %>%
      group_map(~{
        index_close = st_nearest_feature(index_segement, .x, pairwise = T)

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
      reduce(bind_rows) %>%
      mutate(segement = paste0(.x[c(1, 2)], collapse = "_")
             ,segement_name = paste0(.x[c(3, 4)], collapse = "_")
             ,segement_type = paste0(.x[c(5, 6)], collapse = "_"))

    # index_segement_inflated = index_segement %>% quick_buffer(rad = 500)
    #
    # min_distance = as.numeric(
    #   st_distance(index_segement_inflated[2, ]
    #               ,index_segement_inflated[1, ])[[1]])
    #
    # temp_processed %>%
    #   st_filter(index_segement_inflated)  %>%
    #   st_drop_geometry() %>%
    #   select(trip_id, direction_id, route_id, date_time, vehicle_id, segement
    #          ,segement_name, segement_type, time_diff, ttl_diff, speed_avg) %>%
    #   group_by(trip_id, direction_id, route_id
    #            ,vehicle_id, segement, segement_name, segement_type) %>%
    #   filter(n() == 2) %>%
    #   ungroup() %>%
    #   filter(!is.na(ttl_diff)) %>%
    #   filter(ttl_diff > min_distance)
  }, .progress = T)

tictoc::toc()


# saveRDS(temp_output_halfMeth, here::here("data/temp_output_halfMeth.rds"))
# temp_output_halfMeth = readRDS(here::here("data/temp_output_halfMeth.rds"))

fltr_rad = 50

temp_output_halfMeth_sf = temp_output_halfMeth %>%
  map(~{
    object = .x

    index_points = object %>%
      pull(segement) %>%
      .[1] %>%
      str_split(. , "_") %>%
      .[[1]]

    # print(index_points)

    index_segement = combined %>%
      filter(index %in% index_points)

    index_segement_inflated = index_segement %>% quick_buffer(rad = fltr_rad)

    min_distance = as.numeric(
      st_distance(index_segement_inflated[2, ]
                  ,index_segement_inflated[1, ])[[1]])

    object %>%
      st_filter(index_segement_inflated)  %>%
      st_drop_geometry() %>%
      select(trip_id, direction_id, route_id, date_time, vehicle_id, segement
             ,segement_name, segement_type, time_diff, ttl_diff, speed_avg) %>%
      group_by(trip_id, direction_id, route_id
               ,vehicle_id, segement, segement_name, segement_type) %>%
      filter(n() == 2) %>%
      ungroup() %>%
      filter(!is.na(ttl_diff)) %>%
      filter(ttl_diff > min_distance)



  })


temp_output_halfMeth_sf %>% map(nrow)


identical(
temp_output,  temp_output)






tictoc::toc()

##individual_segment========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp_output_pro = temp_output_halfMeth_sf  %>%
  reduce(bind_rows) %>%
  mutate(flag_peak = case_when(
    hour(date_time) >= 6 &
      hour(date_time) < 9 ~ "AMPeak"
    ,hour(date_time) >= 15 &
      hour(date_time) < 19 ~ "PMPeak"
    ,T~"NotTracked"
  )) %>%
  # mutate(segement_name = fct_relevel(
  #   segement_name
  #   ,"23rd_Hewitt", "18th_23rd"
  #   ,"14th_18th", "ramp_14th"
  #   ,"4th_ramp", "grove_4th"
  #   ,"88th_grove", "100th_88th"
  # )) %>%
  filter(flag_peak != "NotTracked") %>%
  filter(speed_avg < 60)

temp_output_pro %>%
  ggplot(aes(
    segement_name
    ,time_diff
    # ,speed_avg
    ,fill = flag_peak
  )) +
  geom_boxplot() +
  facet_grid(
    rows = vars(direction_id)
             ) +
  labs(
    x = "Segment"
    ,y = "Segment Travel Time"
    ,fill = "Peak"
  ) +
  coord_cartesian(ylim = c(0
                           ,1500
                           # ,40
                           ))


temp_output_pro %>%
  ggplot(
    aes(
      # segement_name
      time_diff
      ,speed_avg
      # ,time_diff
      ,color = flag_peak
      ,group = segement_name
    )) +
  geom_point() +
  # geom_smooth(method = "gam") +
  coord_cartesian(ylim = c(0, NA))
facet_grid(cols = vars(segement_name))


##end=====


##modeling========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                                                                                                                                                                                                                                                                                                                                    ##end

#this is a different temp_output_pro than above - so have to remake or rename
temp_output_pro = temp_output_halfMeth_sf %>% reduce(bind_rows)
# temp_output_pro = temp_output %>% reduce(bind_rows)

temp_output_pro_comb =
  merge(
    temp_output_pro %>%
      filter(segement_type != "end_end")
    ,temp_output_pro %>%
      filter(segement_type == "end_end") %>%
      rename_at(vars(segement:speed_avg, date_time), ~paste0(.x, "_full"))
    ,by = c("trip_id", "direction_id", "route_id", "vehicle_id")
    # ,by.y = c("trip_id_full", "direction_id_full", "route_id_full", "vehicle_id_full")
  )


# yolo = temp_output_pro_comb %>%
#   # filter(hour(date_time) >= 15 &
#   #          hour(date_time) < 18
#   # ) %>%
#   # filter(abs(scale(time_diff)) < 3) %>%
#   # filter(abs(scale(time_diff_full)) < 3) %>%
#   # filter(abs(scale(speed_avg_full)) < 3) %>%
#   ggplot(aes((time_diff), (time_diff_full)
#              ,color = scale(speed_avg_full)
#              ,group = segement_name)) +
#   geom_point() +
#   # geom_smooth(method = "lm") +
#   # coord_equal() +
#   geom_abline(slope = 1) +
#   facet_grid(cols = vars(segement_name))
#
#
# yolo %>%
#   plotly::ggplotly()
#
# ggplot(speed_avg_full) +
#   geom_density(aes(speed))

mod_data = temp_output_pro_comb %>%
  filter(speed_avg < 80
         ,speed_avg_full < 80) %>%
  # filter(hour(date_time) >= 13 &
  #          hour(date_time) < 18)
  group_by(direction_id, segement_name) %>%
  filter(abs(scale(time_diff)) < 2) %>%
  filter(abs(scale(time_diff_full)) < 3) %>%
  # filter(abs(scale(speed_avg_full)) < 3)
  mutate



mod_data %>%
  filter(speed_avg < 80
         ,speed_avg_full < 80) %>%

  # ggpa
  pivot_longer(cols = c(time_diff, time_diff_full, speed_avg_full)) %>%
  ggplot() +
  geom_density(aes(value)) +
  facet_grid(cols = vars(name), scales = "free")




mod_data %>%
  select(segement_name, contains("speed"), contains("diff")) %>%
  group_by(segement_name) %>%
  skimr::skim()

mod_data %>%
  filter(segement_name == "14th_Hewitt") %>%
  # filter(segement_name %in% sample(index_seg_names$segement_name, 3)) %>%
  # ggplot(aes(time_diff, time_diff_full-mean(time_diff_full))) +
  ggplot(
    aes(
      scale(time_diff)
      ,scale((time_diff_full
              #-mean(time_diff_full)
              ))
    )) +
  geom_point() +
  geom_smooth(method = "glm") +
  geom_abline(slope = 1) +
  facet_grid(cols = vars(segement_name, direction_id))

index_seg_names = mod_data %>%  select(segement_name) %>% unique()

model_smmry = crossing(
  segment_name = index_seg_names$segement_name
  ,direction = c("north_bound", "south_bound")
) %>%
  split(., rownames(.)) %>%
  # .[1] %>%
  map(~{

    object = .x

    mod_smmry = lm((time_diff_full)~time_diff*as.factor(hour(date_time_full))
                   ,data = mod_data %>%
                     filter(segement_name == object$segment_name) %>%
                     filter(direction_id == object$direction)

    )
    #   summary()
    #
    # coefficients(mod_smmry) %>%
    #   data.frame() %>% rownames_to_column(var = "coef") %>%
    #   janitor::clean_names() %>%
    #   mutate(segment_name = object$segment_name,
    #          direction = object$direction)
    })


# model_smmry = crossing(
#   segment_name = index_seg_names$segement_name
#   ,direction = c("north_bound", "south_bound")
# ) %>%
#   split(., rownames(.)) %>%
#   # .[1] %>%
#   mutate(
#     test = map(~{
#
#     object = .x
#
#     mod_smmry = lm((time_diff_full)~time_diff*as.factor(hour(date_time_full))
#                    ,data = mod_data %>%
#                      filter(segement_name == object$segment_name) %>%
#                      filter(direction_id == object$direction)
#
#     )
#     #   summary()
#     #
#     # coefficients(mod_smmry) %>%
#     #   data.frame() %>% rownames_to_column(var = "coef") %>%
#     #   janitor::clean_names() %>%
#     #   mutate(segment_name = object$segment_name,
#     #          direction = object$direction)
#   })





  mod_nest = index_seg_names %>%
    mutate(seg_comb = str_glue("{segement_name}-{direction_id}")) %>%
    group_by(seg_comb) %>%
    nest() %>%
    mutate(
      mod =map(
          data,
          ~{
            mod_smmry = lm((time_diff_full)~time_diff*as.factor(hour(date_time_full))
                           ,data = mod_data %>%
                             filter(segement_name == .x$segement_name) %>%
                             filter(direction_id == .x$direction_id)
            )
          })
    ) %>%
    mutate(mod_results = map(mod, broom::glance)) %>%
    mutate(mod_coef = map(mod, broom::tidy))










yolo = mod_nest %>%
  unnest(cols = c("data", "mod_coef")) %>%
  filter(str_detect(term, "time_diff")) %>%
  ggplot() +
  geom_point(aes(
    segement_name
    ,term
    ,color = dgt2(estimate)
    ,shape = p.value < .05
    ,size = p.value < .05
  )) +
  facet_grid(cols = vars(direction_id)) +
  ggplot2::theme_classic()

yolo %>% ggplotly()



