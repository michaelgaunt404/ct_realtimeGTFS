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

filter_object = gtfs_pro[["shapes"]]
routes = c("202", "201")
start_only = T
crs_to = 4326
crs_with = 32610
buffer_radius = 100

data = here::here(
  "data/Turning Movement Counts_Fall_2022_Snohomish County, WA"
  ,"Turning Movement Counts_Fall_2022_Snohomish County, WA.csv") %>%
  read_csv()

data_sf = data %>%
  st_as_sf(wkt = "intersection_id_geom", crs = 4326) %>%
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

data_sf %>%
  filter(intersection_id %in% data_sf_fltrd_sf$intersection_id) %>%
  st_drop_geometry() %>%
  write_csv(here::here("data/Turning Movement Counts_Fall_2022_Snohomish County, WA"
                       ,"snoho_tmc_fltrd.csv"))



data_sf_fltrd_sf =  data_sf %>%
  group_by(intersection_id_1 = intersection_id) %>%
  group_map({~head(.x, 1)}) %>%
  reduce(bind_rows) %>%
  mutate(intersection_name = str_glue("{inbound_street_name} - {outbound_street_name}")) %>%
  st_filter(filter_object_buf) %>%
  select(intersection_id, intersection_name)

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

##sub header 1==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub header 2==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#script end=====================================================================










































