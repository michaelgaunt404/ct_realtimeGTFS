check_rtgtfs_trip_vehicle_ids = function(data){
  tmp_data = data %>%
    select(trip_id, direction_id, route_id, start_date, vehicle_id) %>%
    unique() %>%
    arrange(trip_id) %>%
    group_by(trip_id) %>%
    mutate(count_trip_id = n()) %>%
    group_by(vehicle_id) %>%
    mutate(count_bus_id = n())

  max(tmp_data$count_trip_id)
}

check_rtgtfs_trip_vehicle_ids(data_vp)
data = data_vp


tmp_data %>%
  arrange(count_trip_id)


data %>%
  filter(trip_id == "11043496__MCOB-DO:121:0:Weekday:2:23MAR:61041:12345")
