library(ggplot2)
library(readr)
library(dplyr)
library(shiny)
library(bslib)
library(shinydashboard)
library(fresh)
library(bcmaps)
library(sf)
library(leaflet)
library(httr)
library(jsonlite)
library(plotly)
library(tidyr)
library(stringr)
library(rsconnect)

source("R/data_helpers.R")

# All externally-sourced data below is read from data/cache/, which is
# populated by data_refresh.R on a schedule. The app itself never
# scrapes or calls an external API directly at startup - if a scheduled
# refresh fails for a dataset, its cache file is left untouched, so the
# app keeps running on the last successful pull (see
# data/cache/refresh_log.csv for a history of refresh attempts).

#CHARGING STATIONS IN T-O
df_stations_TO <- read_cache_csv("charging_stations")

df_freshness <- read_cache_csv("charging_stations_freshness")

#ELECTRICITY GENERATION TECH 
df_genTech <- read_cache_csv("generation_tech")
df_genTech2023 <- df_genTech %>%
  filter(Year <= 2023)

#END USE DEMAND - TRANSPORTATION 
df_end_use <- read_cache_csv("end_use")
df_end_use2023 <- df_end_use %>%
  filter(Year <= 2023)

#ELECTRICITY DATE MODIFIED
df_energy_date <- read_cache_csv("energy_modified")

#THOMPSON OKANAGAN BOUNDRY
df_TO_boundary <- st_read("data/thompson_okanagan_boundary.geojson")
