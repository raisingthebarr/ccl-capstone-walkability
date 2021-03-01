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

#setting working directory
setwd("C:/Users/Anumita Jain/Desktop/Junior Year/ccl-capstone-walkability/Data")

#loading data-----
sidewalks <- st_read("sidewalks", "sidewalks")
lrtstations <- st_read("lrtstations", "lrtstation")
lrtlines <- st_read("lrtlines", "lrtlines")
bikeways <- st_read("bikeways", "bikeways")
zones <- st_read("zones", "ElementaryAttendanceZones1920")
busstops <- st_read("busstops", "busstops")
busroutes <- st_read("busroutes", "busroutes")

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

#notes on shapes-----
#sidewalks = multilinestring
#lrtstations = point
#lrtlines = linestring
#bikeways = multilinestring
#busstops = point
#busroutes = multilinestring
#zones = polygon

#calculating percentage -----

zoneslrts <- st_join(lrtstations, zones)
rankinglrts <- tabyl(zoneslrts, Elementary)

zoneslrtl <- st_intersection(lrtlines,zones)
rankinglrtl <- tapply(st_length(zoneslrtl), zoneslrtl$Elementary,sum)
View(rankinglrtl)

zonesbus <- st_join(busstops, zones)
rankingbus <- tabyl(zonesbus, Elementary)

zonesbusr <- st_intersection(busroutes, zones)
rankingbusr <- tapply(st_length(zonesbusr), zonesbusr$Elementary, sum)
View(rankingbusr)

zonesbike <- st_join(bikeways, zones)
rankingbike <- tapply(st_length(zonesbike), zonesbike$Elementary, sum)
View(rankingbike)

zonessw <- st_join(sidewalks,zones)
rankingsidewalk <- tapply(st_length(zonessw), zonessw$Elementary,sum)
View(rankingsidewalk)

