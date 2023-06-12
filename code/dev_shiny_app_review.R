library(crosstalk)
library(dplyr)
library(leaflet)
library(leaflet.extras)

# Create a sample data frame with route_id, direction, time of day, latitude, longitude, and speed
data <- data.frame(
  route_id = c("A", "A", "B", "B", "C", "C"),
  direction = c("North", "South", "East", "West", "North", "South"),
  time_of_day = c("Morning", "Afternoon", "Morning", "Afternoon", "Morning", "Afternoon"),
  latitude = c(51.5, 51.6, 51.7, 51.8, 51.9, 52.0),
  longitude = c(-0.1, -0.2, -0.3, -0.4, -0.5, -0.6),
  speed = c(60, 70, 55, 65, 50, 45)
)

data = data_vp_sf %>%
  mutate(hour = hour(date_time))
data_vp_sf %>%  names()
# Create a SharedData object for filtering
shared_data <- SharedData$new(data)


# Create a filter for route_id
filter_route = filter_select(
  id = "id_route", label = "Route", shared_data, ~route_id
)

# Create a filter for direction
filter_direction = filter_checkbox(
  id = "id_direction", label = "Direction", shared_data, ~direction_id, inline = T
)

# Create a filter for time of day
filter_time = filter_slider(
  id = "id_tod", label = "Time of Day", shared_data, ~hour
)

# Create a Leaflet map
leaflet_map <- leaflet(shared_data) %>%
  addTiles() %>%
  addCircleMarkers(
     color = "black", weight = 1
    ,fillColor = ~colorNumeric(palette = "YlOrRd", domain = c(0, 100))(speed_avg),
    fillOpacity = 0.7
  ) %>%
  addLegend(
    position = "topright",
    pal = colorNumeric(palette = "YlOrRd", domain = c(0, 100)),
    values = ~speed_avg,
    title = "Speed (mph)"
  )



bscols(widths = c(2, 4, 6, 12)
         ,filter_direction
       ,filter_route
         ,filter_time
       ,leaflet_map)




# Create a crosstalk group
# group <- crosstalk::group("mygroup")
#
# # Create a crosstalk dashboard
# dashboard <- dashboardBody(
#   fluidRow(
#     column(3, filter_route, filter_direction, filter_time),
#     column(9, leaflet_map)
#   )
# )
#
# # Create a shiny app
# shinyApp(
#   ui = dashboardPage(dashboard),
#   server = function(input, output, session) {
#     # Link the filters to the map
#     filtered_data <- reactive({
#       shared_data$subset(
#         route_id %in% input$filter_route &
#           direction %in% input$filter_direction &
#           time_of_day %in% input$filter_time
#       )
#     })
#     leafletProxy("shared_data") %>%
#       clearMarkers() %>%
#       addCircleMarkers(
#         data = filtered_data(),
#         radius = 5,
#         color = "black",
#         fillColor = ~colorNumeric(palette = "YlOrRd", domain = c(0, 100))(speed),
#         fillOpacity = 0.7
#       )
#   },
#   options = list(height = "100%")
# )
