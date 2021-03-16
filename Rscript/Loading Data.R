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


#notes on shapes-----
#sidewalks = multilinestring
#lrtstations = point
#lrtlines = linestring
#bikeways = multilinestring
#busstops = point
#busroutes = multilinestring
#zones = polygon

#experimenting with opentripplanner-----
library(rJava)
library(opentripplanner)


otp_check_java()

path_data <- file.path("C:/Users/Anumita Jain/Desktop/Junior Year/ccl-capstone-walkability/Data", "OTP")
dir.create(path_data)
path_otp <- otp_dl_jar(path_data, cache = FALSE)
log1 <- otp_build_graph(otp = path_otp , dir = path_data) 
log2 <- otp_setup(otp = path_otp, dir = path_data)
otpcon <- otp_connect()
otp_plan(otp_connect, fromPlace = c(29.722388419368436, -95.40204852710573),
         toPlace = c(29.727476254276088, -95.40782504533031))


path_data <- file.path("C:/Users/Anumita Jain/Desktop/Junior Year/ccl-capstone-walkability/Data", "OTP")
dir.create(path_data) 
path_otp <- otp_dl_jar(path_data, cache = FALSE)
log1 <- otp_build_graph(otp = path_otp, dir = path_data) 
log2 <- otp_setup(otp = path_otp, dir = path_data)
otpcon <- otp_connect()
route <- otp_plan(otpcon, 
                  fromPlace = c(), 
                  toPlace = c())

#calculating percentage -----

zoneslrts <- st_join(lrtstations, zones)
rankinglrts <- tabyl(zoneslrts, Elementary)
rankinglrts$area <- zones$Shape_Area
rankinglrts <- transform(rankinglrts, percent = n / area)

zoneslrtl <- st_intersection(lrtlines,zones)
rankinglrtl <- tapply(st_length(zoneslrtl), zoneslrtl$Elementary,sum)
rankinglrtl <- as.data.frame(rankinglrtl)
rankinglrtl[is.na(rankinglrtl)] <- "0"
names(rankinglrtl)[names(rankinglrtl) == "rankinglrtl"] <- "length"
rankinglrtl <- data.frame(rankinglrtl$length, zones$Elementary, zones$Shape_Area)
rankinglrtl$rankinglrtl.length <- as.numeric(rankinglrtl$rankinglrtl.length)
rankinglrtl <- transform(rankinglrtl, percent = rankinglrtl.length / zones.Shape_Area)

zonesbus <- st_join(busstops, zones)
rankingbus <- tabyl(zonesbus, Elementary)
rankingbus <- na.omit(rankingbus)
rankingbus$area <- zones$Shape_Area
rankingbus <- transform(rankingbus, percentbus = n / area)

zonesbusr <- st_intersection(busroutes, zones)
rankingbusr <- tapply(st_length(zonesbusr), zonesbusr$Elementary, sum)
rankingbusr <- as.data.frame(rankingbusr)
rankingbusr[is.na(rankingbusr)] <- "0"
names(rankingbusr)[names(rankingbusr) == "rankingbusr"] <- "length"
rankingbusr <- data.frame(rankingbusr$length, zones$Elementary, zones$Shape_Area)
rankingbusr$rankingbusr.length <- as.numeric(rankingbusr$rankingbusr.length)
rankingbusr <- transform(rankingbusr, percent = rankingbusr.length / zones.Shape_Area)

zonesbike <- st_join(bikeways, zones)
rankingbike <- tapply(st_length(zonesbike), zonesbike$Elementary, sum)
rankingbike <- as.data.frame(rankingbike)
rankingbike[is.na(rankingbike)] <- "0"
names(rankingbike)[names(rankingbike) == "rankingbike"] <- "length"
rankingbike <- data.frame(rankingbike$length, zones$Elementary, zones$Shape_Area)
rankingbike$rankingbike.length <- as.numeric(rankingbike$rankingbike.length)
rankingbike <- transform(rankingbike, percent = rankingbike.length / zones.Shape_Area)

zonessw <- st_join(sidewalks,zones)
rankingsw <- tapply(st_length(zonessw), zonessw$Elementary,sum)
rankingsw <- as.data.frame(rankingsw)
rankingsw[is.na(rankingsw)] <- "0"
names(rankingsw)[names(rankingsw) == "rankingsw"] <- "length"
rankingsw <- data.frame(rankingsw$length, zones$Elementary, zones$Shape_Area)
rankingsw$rankingsw.length <- as.numeric(rankingsw$rankingsw.length)
rankingsw <- transform(rankingsw, percent = rankingsw.length / zones.Shape_Area)


#trying to create 2-mile isochrone map-----
library(devtools)
library(osrm)

poe <- osrm::osrmIsometric(
  loc = c(29.7274389868503, -95.40736370540988),
  breaks = seq(from = 0.1, to = 3218, length.out = 4),
  exclude = NULL,
  res = 30,
  returnclass = "sf",
  osrm.server = getOption("osrm.server"),
  osrm.profile = getOption("foot")
)

