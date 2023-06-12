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
library(magrittr)
library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(jsonlite)
library(rtgtfsr)
library(log4r)
library(sf)
library(leaflet)
library(leafpop)
library(tidytransit)

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

message("Proceeding to download GTFS files using provided URL")
gtfs_path = "https://www.communitytransit.org/docs/default-source/open-data/gtfs/current.zip"
gtfs = tidytransit::read_gtfs(gtfs_path)
gtfs_pro = tidytransit::gtfs_as_sf(gtfs)



filter_object = here::here("data/WSDOT_-_Traffic_Sections_(AADT)"
                           ,"WSDOT_-_Traffic_Sections_(AADT).shp") %>%
  read_sf() %>%
  gauntlet::st_true_midpoint()


filter_object %>% mapview::mapview()


filter_object = gtfs_pro[["shapes"]]
routes = c("202", "201")
start_only = T
crs_to = 4326
crs_with = 32610
buffer_radius = 100

data = here::here(
  "data/Annual Average Daily Traffic (AADT)_2022_Snohomish County, WA"
  ,"Annual Average Daily Traffic (AADT)_2022_Snohomish County, WA.csv") %>%
  read_csv()

data_sf = data %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  st_make_valid()

if (start_only) {
  filter_object = filter_object %>%
    filter(str_detect(shape_id, paste0(routes, ":", collapse = "|")))
}

filter_object_buf = filter_object %>%
  quick_buffer(to = crs_to, with = crs_with, radius = buffer_radius) %>%
  st_union() %>%
  st_make_valid()

# filter_object_buf %>%
#   mapview::mapview()
#

# data_sf %>%
#   filter(intersection_id %in% data_sf_fltrd_sf$intersection_id) %>%
#   st_drop_geometry() %>%
#   write_csv(here::here("data/Turning Movement Counts_Fall_2022_Snohomish County, WA"
#                        ,"snoho_tmc_fltrd.csv"))

data_sf_fltrd_sf %>%
  st_drop_geometry() %>%
  write.csv(here::here("data/Annual Average Daily Traffic (AADT)_2022_Snohomish County, WA"
                      ,"corridor_aadt_fltrd.csv"))


mapview::mapviewOptions(homebutton = 'F')
data_sf_fltrd_sf =  data_sf %>%
  st_filter(filter_object_buf)

tmp_map = data_sf_fltrd_sf %>%
  filter(highway != "motorway") %>%
  group_by(highway_1 = highway) %>%
  group_map(~{
    tmp = unique(.x$highway)

    mapview::mapview(.x, zcol = "aadt", lwd = "aadt", layer.name = tmp)
  }) %>%
  reduce(`+`)

tmp_map@map = tmp_map@map %>%
  leaflet::hideGroup(c("living_street", "motorway_link", "service", "track"))

data_sf_fltrd = data_sf_fltrd_sf %>%
  st_drop_geometry()

list_tmc_plots = data %>%
  filter(intersection_id %in% data_sf_fltrd$intersection_id) %>%
  filter(day %in% c("TUE", "WED", "THU")) %>%
  group_by(intersection_id) %>%
  group_map(~{
    tmp_plot =
      .x %>%
      group_by(inbound_street_name, inbound_direction, hour, turn_maneuver) %>%
      summarise(count_avg = mean(count)) %>%
      ggplot() +
      geom_line(aes(hour, count_avg, group = turn_maneuver)
      ) +
      facet_grid(
        cols = vars(inbound_street_name, inbound_direction)
        ,rows = vars(turn_maneuver)
        ,scales = "free") +
      xlim(c(0, NA)) +
      labs(x = "Time of Day (Hour)", y = "", color = "Movement")

  })

int_map =
  leaflet(height = 700) %>%
  leaflet::addTiles(group = "OSM (default)") %>%
  leaflet_default_tiles() %>%
  gauntlet::leaflet_default_tiles() %>%
  addPolygons(data = filter_object_buf, group = "Study Area"
              ,color = "black", opacity = 1, weight = 1, fillColor = "blue", fillOpacity = .1) %>%
  addCircleMarkers(data = data_sf_fltrd_sf
                   ,group = "Intersections"
                   ,color = "black", opacity = .5, weight = 2, fillColor = "blue"
                   ,label = data_sf_fltrd_sf$intersection_name %>%
                     map(htmltools::HTML)
                   , popup= leafpop::popupGraph(list_tmc_plots
                                                ,width = 800
                                                ,height = 300)) %>%
  leaflet::addLayersControl(
    baseGroups = leaflet_default_tiles_index()
    ,overlayGroups =
      c("Study Area", "Intersections")
    ,options = layersControlOptions(collapsed = F, sortLayers = F))


# int_map %>%
#   htmlwidgets::saveWidget(here::here("viz", "example_tmc_map.html"))

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_sf_joined = data_sf %>%
  filter(!is.na(highway)) %>%
  filter(!(highway %in% c("living_street", "motorway", "residential"
                          ,"motorway_link", "service", "track"))
         ,!str_detect(highway, "link")) %>%
  st_join(filter_object %>%
            quick_buffer(radius = 2))

data_sf_joined_pro = data_sf_joined %>%
  filter(!is.na(aadt)
         ,!is.na(AADT))

(data_sf_joined_pro %>%
  mutate(metric = dgt2(abs((aadt-AADT)/AADT))) %>%
    select(AADT, aadt, starts_with("aadt"), metric, bidirectional, highway, street_name, Location, StateRoute) %>%
  mapview::mapview(zcol = "metric", lwd = "metric", layer.name = "TMC")) +
  (filter_object %>%
     filter(GlobalID %in% data_sf_joined_pro$GlobalID) %>%
     mapview::mapview())



data_joined_pro = data_sf_joined_pro %>%
  st_drop_geometry()

data_sf_joined_pro %>%
  # group_by(GlobalID) %>%
  # filter(n()>1) %>%
  ggplot() +
  geom_point(aes(AADT, aadt
                 # ,color = abs((aadt-AADT)/AADT)
                 ,color = bidirectional
                 )) +
  geom_abline(intercept = 0) +
  geom_abline(intercept = 0, slope = .5) +
  facet_wrap(vars(highway))

aadt_comp = data_sf_joined %>%
  filter(!is.na(AADT)) %>%
  filter(!(highway %in% c("living_street", "motorway", "motorway_link", "service", "track"))) %>%
  st_drop_geometry()

  model = lm(AADT~aadt+bidirectional, aadt_comp)

  model %>% summary()
  plot(model)
  d<-density(model[['residuals']])
  plot(d,main='Residual KDE Plot',xlab='Residual value')

  ##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































