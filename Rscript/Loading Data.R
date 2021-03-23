#loading data and packages

rm(list = ls())

#loading packages-----
library(rlang)
library(tidyverse)
library(janitor)
library(raster)
library(rgdal)
library(mapview)
library(leaflet)
library(sf)
library(tigris)
library(tidycensus)
library(ggplot2)
library(sp)
library(dplyr)
library(maptools)
library(opentripplanner)

#setting working directory
setwd("C:/Users/Anumita Jain/Desktop/Junior Year/ccl-capstone-walkability/Data")


#loading data-----
sidewalks <- st_read("sidewalks", "sidewalks")
lrtstations <- st_read("lrtstations", "lrtstation")
lrtlines <- st_read("lrtlines", "lrtlines")
bikeways <- st_read("bikeways", "bikeways")
busstops <- st_read("busstops", "busstops")
busroutes <- st_read("busroutes", "busroutes")
zones <- st_read("zones", "ElementaryAttendanceZones1920")
zones <- zones[order(zones$Elementary),]
load("C:/Users/Anumita Jain/Downloads/hcad-parcels.RData")
schools <- st_read("schools", "schools")
schools <- subset(schools, DistName == "HOUSTON ISD")


#cleaning data-----
clean_shape <- function(.data, .crs=nad83) {
  nad83<-st_crs("+proj=longlat +ellps=WGS84 +datum=NAD83 +no_defs +towgs84=0,0,0")
  
  data <- .data %>% 
    clean_names() %>% 
    st_transform(., .crs)
  
  return(data)
}

clean_shape(sidewalks)
clean_shape(lrtstations)
clean_shape(lrtlines)
clean_shape(bikeways)
clean_shape(zones)
clean_shape(busstops)
clean_shape(busroutes)
clean_shape(schools)
parcels <- clean_shape(parcels_full)
zones <- clean_shape(zones)


#intersecting parcels with zones----
parcels_full$geom2 <- st_centroid(st_geometry(parcels_full))
View(parcels_full$geom2, parcels_full$geom)

rightcrs <- st_crs(zones)
st_transform(parcels_full, crs = st_crs(zones))
st_transform(zones, crs = st_crs(parcels_full))
parcels_full$geom <- st_cast(parcels_full$geometry, "POINT")


clean_shape(parcels_full)

parcels_full %>% st_set_crs(4326) %>% st_transform(NAD83)

parcels_full <- st_as_sf(parcels_full)

zoneparcel <- st_join(zones, parcels_full)
st_crs(parcels_full)
st_crs(zones)

#experimenting with opentripplanner-----
library(opentripplanner)


otp_check_java()

library(opentripplanner)
path_data <- file.path("C:/Users/Anumita Jain/Desktop/Junior Year/ccl-capstone-walkability/Data", "OTP")
path_otp <- otp_dl_jar()

log1 <- otp_build_graph(otp=path_otp, dir = path_data)
log2 <- otp_setup(otp = path_otp, dir = path_data)
otpcon <- otp_connect()
route <- otp_plan(otpcon,
                  fromPlace = c(-95.3890974,29.7868162),
                  toPlace = c(-95.40736370540988, 29.7274389868503))
route
