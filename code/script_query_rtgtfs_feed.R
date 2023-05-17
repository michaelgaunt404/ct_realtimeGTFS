#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script queries RTGTFS feeds.
#
# By: mike gaunt, mike.gaunt.404@gmail.com
#
# README: You need to change API urls
#-------- You will need to crete a blank logfile before hand
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
# library(gauntlet)
library(here)
library(magrittr)
library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(jsonlite)
library(rtgtfsr)
library(log4r)
library(gauntlet)

#import data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#manual step to send/receive data from Google drive location
#this is ran on mikes local computer

#helpful targets functions======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#manual step to send/receive data from Google drive location

#SECTION: Run query and process=================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
query_rtgtfs_json(
  rtgtfs_feeds = list(
    url_updates = 'http://s3.amazonaws.com/commtrans-realtime-prod/tripupdates_pb.json'
    ,url_vp = 'http://s3.amazonaws.com/commtrans-realtime-prod/vehiclepositions_pb.json'
    ,url_alerts = 'http://s3.amazonaws.com/commtrans-realtime-prod/alerts_pb.json'
  )
  ,ttl_query_duration = 1  # Hours
  ,cache_interval = 15  # Minutes
  ,query_interval = 10  # Seconds
  ,routes = c("201", "202")
  ,log_file = "logfile_ct_rtgtfs.txt"
)

#script end=====================================================================


