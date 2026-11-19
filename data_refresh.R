# Data refresh job.
#
# Fetches every externally-sourced dataset the app depends on and writes
# each one to data/cache/. Each dataset is fetched and written
# independently: if one source fails (site down, API change, network
# blip), that dataset's existing cache file is left untouched and the
# rest of the refresh continues. global.R only ever reads from
# data/cache/, so the app always has the most recent successful pull
# of each dataset, even if today's refresh partially failed.
#
# Run manually with:
#   Rscript data_refresh.R
#
# To run this once a day.
# (e.g.GitHub Actions)

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(geojsonsf)
library(rvest)
library(chromote)
source(".Renviron")

source("R/data_helpers.R")

clear_refresh_log()

refresh_dataset <- function(name, fetch, write) {
  message("Refreshing ", name, "...")
  tryCatch(
    {
      value <- fetch()
      write(value)
      log_refresh(name, "success", n_rows = NROW(value))
      message("  OK (", NROW(value), " rows)")
      TRUE
    },
    error = function(e) {
      log_refresh(name, "failure", message = conditionMessage(e))
      message("  FAILED - keeping previous cache: ", conditionMessage(e))
      FALSE
    }
  )
}


#CHARGING STATIONS IN T-O REGION ---------------------------------------------------------------------------
refresh_dataset(
  "charging_stations",
  function() {
    APIKEY <- Sys.getenv("APIKEY")
    url <- paste0(
      "https://developer.nlr.gov/api/alt-fuel-stations/v1.geojson?api_key=", APIKEY, "&fuel_type=ELEC&state=BC&country=CA"
    )
    
    charging_stations <- st_read(url, quiet = TRUE)
    
    station_sf <- st_as_sf(
      charging_stations,
      crs = 4326,
    )
    
    region <- st_read("data/thompson_okanagan_boundary.geojson", quiet = TRUE)
    
    region <- st_transform(region, st_crs(station_sf))
    
    # Keep only locations inside the tourism region
    station_sf <- st_filter(
      station_sf,
      region,
      .predicate = st_covered_by
    )
    
    coords <- st_coordinates(station_sf)
    
    station_sf$longitude <- coords[, "X"]
    station_sf$latitude <- coords[, "Y"]
    
    # Remove geometry before saving
    st_drop_geometry(station_sf)
    
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "charging_stations.csv"))
)

#LAST UPDATED CHARGING STATIONS
refresh_dataset(
  "charging_stations_freshness",
  function() {
    APIKEY <- Sys.getenv("APIKEY")
   data <- fromJSON(paste0("https://developer.nlr.gov/api/alt-fuel-stations/v1/last-updated.json?api_key=", APIKEY))
   df <- as.data.frame(data)
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "charging_stations_freshness.csv"))
)

#ELECTRICITY GENERATION BY TECH (RENEWABLE vs FOSSIL-FUEL)
refresh_dataset(
  "generation_tech",
  function() {
    df <- read.csv("https://www.cer-rec.gc.ca/open/energy/energyfutures2026/electricity-generation-technology-2026.csv") #This url will have to be manually updated when future data is released.
    df <- df %>%
      filter(Region == "British Columbia", Scenario == "Current Measures") 
    df
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "generation_tech.csv"))
)

#END USE DEMAND - TRANSPORTATION
refresh_dataset(
  "end_use",
  function() {
    df <- read.csv("https://www.cer-rec.gc.ca/open/energy/energyfutures2026/end-use-demand-2026.csv") #This url will have to be manually updated when future data is released.
    df <- df %>%
      filter(Region == "British Columbia", Scenario == "Current Measures") 
    df
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "end_use.csv"))
)

#ENERGY DATE MODIFIED
refresh_dataset(
  "energy_modified",
  function() {
    data <- fromJSON("https://open.canada.ca/data/api/action/package_show?id=07c42deb-9435-43b9-a416-7ce316f3893d") 
    df <- as.data.frame(data$result$date_modified[1])
  },
  function(df) atomic_write_csv(df, file.path(CACHE_DIR, "energy_modified.csv"))
)

message("Data refresh complete. See ", file.path(CACHE_DIR, "refresh_log.csv"), " for a run history.")