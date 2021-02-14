#loading data and packages

rm(list = ls())

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

setwd("C:/Users/Anumita Jain/Desktop/Junior Year/ccl-capstone-walkability/Data")

sidewalks <- st_read("sidewalks", "sidewalks")
lrtstations <- st_read("lrtstations", "lrtstation")
lrtlines <- st_read("lrtlines", "lrtlines")
bikeways <- st_read("bikeways", "bikeways")
zones <- st_read("zones", "ElementaryAttendanceZones1920")
busstops <- st_read("busstops", "busstops")
busroutes <- st_read("busroutes", "busroutes")

clean_shape <- function(.data, .crs=nad83) {
  nad83<-st_crs("+proj=longlat +ellps=WGS84 +datum=NAD83 +no_defs +towgs84=0,0,0")
  
  data <- .data %>% 
    clean_names() %>% 
    st_transform(., .crs)
  
  return(data)
}

roberts <- subset(zones, OBJECTID_1 == 152)

bounds <- extent(roberts)
lon_min <- as.numeric(bounds@xmin)
lon_max <- as.numeric(bounds@xmax)
lat_min <- as.numeric(bounds@ymin)
lat_max <- as.numeric(bounds@ymax)

bbox <- st_bbox(c(xmin = lon_min, xmax = lon_max, 
                  ymax = lat_max, ymin = lat_min), crs = st_crs(roberts))

robertslrt <- st_intersects(lrtstations, bbox)
robertslrt1 <- st_intersects(lrtstations, roberts)
