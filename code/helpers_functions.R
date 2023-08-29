
# make_vp_data_smmry_stats = function(data){
#   data %>%
#     count(query_batch, route_id, vehicle_id) %>%
#     group_by(query_batch, route_id) %>%
#     summarise(buses_in_service= n()
#               ,total_records = sum(n)) %>%
#     ungroup() %>%
#     mutate(time = hms::as_hms(floor_date(query_batch, "minute"))
#            ,date = date(query_batch))
# }

# plot_query_batch_data =  function(data, days = 7){
#   data %>%
#     count(time = hms::as_hms(floor_date(date_time, "minute"))
#           ,date = date(date_time)) %>%
#     {if(!is.na(days)) (.) %>%
#         filter(date > max(date)-days) else .} %>%
#     ggplot() +
#     geom_tile(aes(time, y = 1,  fill = n)) +
#     facet_grid(row = vars(date), switch ="y") +
#     scale_x_time(breaks = scales::breaks_width("1 hour")) +
#     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#     theme(strip.text.y.left = element_text(angle = 0)) +
#     theme(axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()) +
#     labs(y= "", fill = "Rec. Count", x = "Time of Day")
# }

# plot_buses_in_operation =  function(data, days = 7){
#   data %>%
#     {if(!is.na(days)) (.) %>%
#         filter(date > max(date)-days) else .} %>%
#     ggplot(aes(time, buses_in_service #, color = as.factor(route_id)
#                , group = date)) +
#     geom_line(alpha = .5) +
#     geom_point(alpha = .5) +
#     scale_x_time(breaks = scales::breaks_width("1 hour")) +
#     facet_grid(#row = vars(date),
#       cols = vars(route_id), switch= "y") +
#     coord_cartesian(ylim = c(0, NA)) +
#     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#     labs(y= "No. of Buses Operating", fill = "Rec. Count", x = "Time of Day")
# }

# query_and_process_gtfs = function(url_gtfs){
#   ct_gtfs = tidytransit::read_gtfs(
#     url_gtfs
#   )
#
#   ct_gtfs_sf = gtfs_as_sf(ct_gtfs)
#
#   return(ct_gtfs_sf)
#
# }

# check_rtgtfs_trip_vehicle_ids = function(data){
#   tmp_data = data %>%
#     select(trip_id, direction_id, route_id, start_date, vehicle_id) %>%
#     unique() %>%
#     arrange(trip_id) %>%
#     group_by(trip_id) %>%
#     mutate(count_trip_id = n()) %>%
#     group_by(vehicle_id) %>%
#     mutate(count_bus_id = n())
#
#   max(tmp_data$count_trip_id)
# }

# get_gtsf_shape_xwalk = function(vp_data, gtfs){
#   index = vp_data %>%
#     pull(trip_id) %>%
#     unique()
#
#   index_shape = gtfs$trips %>%
#     filter(trip_id %in% index) %>%
#     select(trip_id, shape_id) %>%
#     unique()
#
#   return(index_shape)
# }

# xwalk_override = function(xwalk_ob, over_ride){
#   if (length(over_ride) == 1){
#     xwalk_ob %>%
#       mutate(shape_id = over_ride)
#   } else {
#     xwalk_ob %>%
#       mutate(shape_id = case_when(
#         gsub(":.*", "\\1", shape_id) == gsub(":.*", "\\1", over_ride[1])~over_ride[1]
#         ,gsub(":.*", "\\1", shape_id) == gsub(":.*", "\\1", over_ride[2])~over_ride[2]
#       ))
#   }}

# query_and_process_gtfs = function(gtfs_path){
#   message("Proceeding to download GTFS files using provided URL")
#   gtfs = tidytransit::read_gtfs(gtfs_path)
#
#   gtfs_pro = tidytransit::gtfs_as_sf(gtfs)
#
#   return(gtfs_pro)
# }

# st_line_sample_to_points = function(sf_object, samp_dist = 100, crs){
#   sf_object_linestring = sf_object %>%
#     mutate(merge_id = row_number()) %>%
#     st_transform(crs) %>%
#     st_cast("LINESTRING") %>%
#     mutate(linestring_id = row_number()) %>%
#     select(merge_id, linestring_id, shape_id)
#
#   sf_object_points = sf_object_linestring %>%
#     st_line_sample(density = 1/samp_dist) %>%
#     st_transform(4326) %>%
#     st_as_sf() %>%
#     bind_cols(sf_object_linestring %>%
#                 st_drop_geometry() %>%
#                 select(shape_id)) %>%
#     st_cast("POINT") %>%
#     mutate(index = row_number()) %>%
#     rename(geometry = x) %>%
#     gauntlet::st_extract_coords() %>%
#     rename(samp_lon = lon, samp_lat = lat)
# }

# make_speed_profiles = function(sf_object, crs, xwalk_ob, vp_data, samp_dist = 100, over_ride = NA){
#
#   if (!is.na(over_ride)) {xwalk_ob = xwalk_override(xwalk_ob, over_ride)}
#
#   sf_object_points = sf_object %>%
#     filter(shape_id %in% unique(xwalk_ob$shape_id)) %>%
#     group_by(shape_id_1 = shape_id) %>%
#     group_map(~st_line_sample_to_points(sf_object = .x, crs = crs, samp_dist = samp_dist)) %>%
#     reduce(bind_rows) %>%
#     remove_rownames()
#
#   temp_speed_profiles = xwalk_ob$shape_id %>%
#     unique() %>%
#     map_df(~{
#
#       print(.x)
#       index_xwalk = xwalk_ob %>%
#         filter(shape_id %in% .x) %>%
#         pull(trip_id)
#
#       temp_vp_data = vp_data %>%
#         filter(trip_id %in% index_xwalk)
#
#       temp_sf_object_points = sf_object_points %>%
#         filter(shape_id %in% .x)
#
#       nrow(temp_sf_object_points) %>%  print()
#       max(temp_sf_object_points$index) %>%  print()
#
#       temp_joined = st_join(x = temp_vp_data
#                             ,y = temp_sf_object_points
#                             ,join = st_nearest_feature) %>%
#         st_drop_geometry()
#     })
#
#   return(list(route_samples = sf_object_points
#               ,speed_profiles = temp_speed_profiles))
# }

# process_vp_list_rds_1 = function(rds_list_objects, timezone = "US/Pacific"
#                                  ,am_peak = c(6, 9), pm_peak = c(16, 18), midday_peak = c(10, 14)){
#   list(rds_list_objects, names(rds_list_objects)) %>%
#     pmap(~{
#       .x %>% mutate(query_batch = .y %>% str_remove_all("data_vp_|\\.rds") %>%
#                       as_datetime(tz = "US/Pacific"))
#     }) %>%
#     reduce(bind_rows) %>%
#     rename_with(~.x %>%
#                   str_replace(.,
#                               "vehicle.id", "vehicle_id") %>%
#                   gsub(".*\\.", "\\1",
#                        .)) %>%
#     arrange(route_id, vehicle_id, trip_id, query_batch) %>%
#     group_by(route_id, vehicle_id, trip_id) %>%
#     mutate(date_time = timestamp %>%
#              as.numeric() %>%
#              lubridate::as_datetime() %>%
#              lubridate::with_tz(timezone)) %>%
#     arrange(route_id, vehicle_id, trip_id, query_batch, date_time) %>%
#     mutate(datetime_diff = as.numeric(date_time - lag(date_time))) %>%
#     ungroup() %>%
#     data.frame() %>%
#     filter(datetime_diff != 0) %>%
#     mutate(flag_peak_time = case_when(
#       hour(date_time)>= am_peak[1] & hour(date_time)< am_peak[2]~"AM Peak"
#       ,hour(date_time)>= pm_peak[1] &hour(date_time)<pm_peak[2]~"PM Peak"
#       ,hour(date_time)>= midday_peak[1] &hour(date_time)<midday_peak[2]~"Midday"
#       ,T~"Untracked"))
# }

# convert_vp_data_to_sf_1 = function (data, crs_to){
#   data %>% sf::st_as_sf(coords = c("longitude", "latitude"),
#                         crs = 4326) %>%
#     sf::st_transform(crs = crs_to) %>%
#     gauntlet::st_extract_coords() %>%
#     group_by(route_id, vehicle_id, trip_id) %>%
#     mutate(lon_diff = lon - lag(lon)
#            ,lat_diff = lat - lag(lat)
#            ,ttl_diff = sqrt(lon_diff^2 + lat_diff^2)
#            ,speed_avg = (ttl_diff/datetime_diff) * 2.236936) %>%
#     ungroup() %>%
#     filter(speed_avg < 90) %>%
#     group_by(route_id, vehicle_id, trip_id, direction_id) %>%
#     mutate(speed_avg_diff = speed_avg-lag(speed_avg)) %>%
#     ungroup() %>%
#     sf::st_transform(4326)
# }

# plot_query_batch_data =  function(data, days = 7){
#   data %>%
#     count(time = hms::as_hms(floor_date(date_time, "minute"))
#           ,date = date(date_time)) %>%
#     {if(!is.na(days)) (.) %>%
#         filter(date > max(date)-days) else .} %>%
#     ggplot() +
#     geom_tile(aes(time, y = 1,  fill = n)) +
#     facet_grid(row = vars(date), switch ="y") +
#     scale_x_time(breaks = scales::breaks_width("1 hour")) +
#     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#     theme(strip.text.y.left = element_text(angle = 0)) +
#     theme(axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()) +
#     labs(y= "", fill = "Rec. Count", x = "Time of Day")
# }
#
# plot_buses_in_operation =  function(data, days = 7){
#   data %>%
#     {if(!is.na(days)) (.) %>%
#         filter(date > max(date)-days) else .} %>%
#     ggplot(aes(time, buses_in_service #, color = as.factor(route_id)
#                , group = date)) +
#     geom_line(alpha = .5) +
#     geom_point(alpha = .5) +
#     scale_x_time(breaks = scales::breaks_width("1 hour")) +
#     facet_grid(#row = vars(date),
#       cols = vars(route_id), switch= "y") +
#     coord_cartesian(ylim = c(0, NA)) +
#     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#     labs(y= "No. of Buses Operating", fill = "Rec. Count", x = "Time of Day")
# }
#
# make_vp_data_smmry_stats = function(data){
#   data %>%
#     count(query_batch, route_id, vehicle_id) %>%
#     group_by(query_batch, route_id) %>%
#     summarise(buses_in_service= n()
#               ,total_records = sum(n)) %>%
#     ungroup() %>%
#     mutate(time = hms::as_hms(floor_date(query_batch, "minute"))
#            ,date = date(query_batch))
# }

# cache_daily_files_1 = function (folder = "data", folder_save_to = "data"){
#   files = list.files(here::here(folder)) %>%
#     gsub(".*data_updates_|data_alerts_|data_vp_"
#          ,"\\1", .) %>%
#     gsub("_.*", "\\1", .) %>%
#     unique() %>%
#     .[!is.na(as.numeric(.))] %>%
#     .[as_date(.) != Sys.Date()]
#
#   files %>%
#     map(~{
#       data_vp = gauntlet::read_rds_allFiles(specifically = str_glue("vp_{.x}"))
#       rds_path = here::here(folder_save_to, str_glue("daily_cache_vp_{.x}.rds"))
#
#       data_vp_comb = data_vp %>% reduce(bind_rows)
#       readr::write_rds(data_vp_comb, rds_path)
#
#       rds_save_ob = readr::read_rds(rds_path)
#       check_row = (nrow(rds_save_ob) == nrow(data_vp_comb))
#       check_identical = identical(rds_save_ob, data_vp_comb)
#     })
# }

folder_with_files = "data/daily_cache_files"
folder_with_files = "data/daily_cache_vp"

send_rds_files_to_gdrive = function(folder_with_files){
  #programmatically sends RDS files to grive
  #--> needs to make temp CSVs for gdrive

  files = here::here(folder_with_files) %>%
    list.files()

  full_path_files = paste0(folder_with_files, "/", files)

  list(
    full_path_files
    ,files) %>%
    pmap(~{

      tryCatch({

        temp_file = tempfile(fileext = ".csv")
        temp_data = readr::read_rds(.x)
        readr::write_csv(temp_data, temp_file)

        # googl
        googledrive::drive_put(
          temp_file
          ,name = paste0(gsub("\\.rds.*", "\\1", .x), ".csv")
          ,type = "csv"
          # ,path = as_id(query_table_sel[['gdrive']]) #I guess you don't really need this
        )

      }, error = function(err) {
        message("An error orccured....")
        print(err$message)
      })
    })

}
data_to_fetch = "data/daily_cache_vp/daily_cache_vp_20230515"
data_to_fetch = "daily_cache_vp/daily_cache_vp_20230515"
data_to_fetch = "daily_cache_vp_20230515"

files = googledrive::drive_find("daily_cache")

files %>%
  split(., rownames(.)) %>%
  map(~{
    print(.x$id[[1]])
  })

data_to_fetch = files %>%
  split(., rownames(.)) %>%
  .[[1]]




tt = "./data/zz_det/yolo.csv" %>%
  read.csv()
saveRDS(tt, "./data/zz_det/yolo.rds")


googledrive::drive_download(
  file = googledrive::as_id("1PrPZ-xvNYglub_m7njpuuPuVXwQiSeZ5")
  ,path = "data/zz_det/yolo.rds", overwrite = T
)




"data/daily_cache_vp/daily_cache_vp_20230515.csv" %>%
  gsub(".*/(.*?)\\.csv$", "\\1", .)














