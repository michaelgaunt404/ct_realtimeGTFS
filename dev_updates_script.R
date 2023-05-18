

30*10

tmp_update = here::here("data/data_updates_20230515_223602.rds") %>%
  read_rds()
gauntlet::get_list_items()

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
    .[1] %>%
    map(~{
      info_trip = .x[[1]] %>% .[[2]] %>% .$trip  %>%  data.frame() #gives trip charactoristics .$stop_time_update %>% .[[2]]

      info_timestamp = .x[[1]] %>% .[[2]] %>% .$timestamp %>% data.frame(timestamp = .)

      info_stops = .x[[1]] %>% .[[2]] %>% .$stop_time_update %>%
        map_df(~.x %>%  flatten_named_list() %>%  pivot_wider())

      bind_cols(info_trip, info_timestamp, info_stops) %>%
        mutate(time_diff = as.numeric(departure.time) - as.numeric(arrival.time)
               ,index = row_number()) %>%
        arrange(timestamp, stop_sequence)

    })
  tmp_update[[1]] %>%  .[[1]] %>% .[[2]] %>% .$trip %>% data.frame()
