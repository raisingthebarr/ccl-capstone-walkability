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

zones <- st_read("zones", "ElementaryAttendanceZones1920")
zones <- zones[order(zones$Elementary),]
load("C:/Users/Anumita Jain/Downloads/hcad-parcels.RData")
isochrones <- st_read("isochrones", "isochrones")
schools <- st_read("schools", "schools")
buszones <- st_read("buszones","buszones")
res <- read.delim("building_res.txt")

res <- res %>% rename(lowparceli = acct)

#cleaning data-----
clean_shape <- function(.data, .crs=nad83) {
  nad83<-st_crs("+proj=longlat +ellps=WGS84 +datum=NAD83 +no_defs +towgs84=0,0,0")
  
  data <- .data %>% 
    clean_names() %>% 
    st_transform(., .crs)
  
  return(data)
}

zones <- clean_shape(zones)
parcel <- clean_shape(parcels_full)
buszones <- clean_shape(buszones)
parcel <- subset(parcel, city == "HOUSTON")
parcel$geom2 <- st_centroid(st_geometry(parcel))




#intersecting parcels with zones----

zoneparcel <- st_join(parcelres, zones)

zoneparcelz <- zoneparcel
zoneparcels <- zoneparcelz[!is.na(zoneparcelz$elementary),]

st_write(zoneparcels, "zoneparcels.shp")
warnings()

View(parcel[449323,])

# creating elementary school objects-----
kolter <- otp_isochrone(otpcon, fromPlace = c(-95.455966499811, 29.7128880002869),
                        mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
kolter$name <- "Kolter"
woodson <- otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
woodson$name <- "Woodson"

markwhite <- otp_isochrone(otpcon, fromPlace = c(-95.5102688042266, 29.7429503699255), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
markwhite$name <- "Mark White"
roberts <- otp_isochrone(otpcon, fromPlace = c(-95.4118697363042, 29.7122312099736), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
roberts$name <- "Roberts"
dezavala <- otp_isochrone(otpcon, fromPlace = c(-95.2890931549106, 29.7373749364491), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
dezavala$name <- "De Zavala"
barrick <- otp_isochrone(otpcon, fromPlace = c(-95.3705438333225, 29.8743280579438), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
barrick$name <- "Barrick"
harvard <- otp_isochrone(otpcon, fromPlace = c(-95.3959673374792, 29.7860210937165), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
harvard$name <- "Harvard"
durkee <- otp_isochrone(otpcon, fromPlace = c(-95.392640890917, 29.8664370549871), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)

durkee$name <- "Durkee"
lantrip <- otp_isochrone(otpcon, fromPlace = c(-95.3352481700697, 29.7406069596261), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
lantrip$name <- "Lantrip"
durham <- otp_isochrone(otpcon, fromPlace = c(-95.4170873933601, 29.8355209468421), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
durham$name <- "Durham"
memorial <- otp_isochrone(otpcon, fromPlace = c(-95.425656188756, 29.7695684903301), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
memorial$name <- "Memorial"
hilliard <- otp_isochrone(otpcon, fromPlace = c(-95.2682076966633, 29.8381284320335), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
hilliard$name <- "Hilliard"
burbank <- otp_isochrone(otpcon, fromPlace = c(-95.3730553184255, 29.8443615332646), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
burbank$name <- "Burbank"
whittier <- otp_isochrone(otpcon, fromPlace = c(-95.2429255610905, 29.7721278746525), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
whittier$name <- "Whittier"
jrharris <- otp_isochrone(otpcon, fromPlace = c(-95.2779396660755, 29.7189984282555), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
jrharris$name <- "John R. Harris"
wesley <- otp_isochrone(otpcon, fromPlace = c(-95.4190381732638, 29.8614750737327), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
wesley$name <- "Wesley"
benavidez <- otp_isochrone(otpcon, fromPlace = c(-95.4921622455329, 29.7173089018659), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
benavidez$name <- "Benavidez"
burnet <- otp_isochrone(otpcon, fromPlace = c(-95.3185539651402, 29.7460714539208), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
burnet$name <- "Burnet"
walnutbend <- otp_isochrone(otpcon, fromPlace = c(-95.5637407429183, 29.7477502406753), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
walnutbend$name <- "Walnut Bend"
wilson <- otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034), 
                            mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
wilson$name <- "Wilson"
dechaumes <- otp_isochrone(otpcon, fromPlace = c(-95.3770576713252, 29.8608399229821), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
dechaumes$name <- "DeChaumes"
gardenoaks <- otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034),
                            mode = c("WALK","BICYCLE","TRANSIT"),cutoffSec = 1800)
gardenoaks$name <- "Garden Oaks"
jefferson <- otp_isochrone(otpcon, fromPlace = c(-95.3707943424291, 29.8051585996561), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
jefferson$name <- "Jefferson"
rodriguez <- otp_isochrone(otpcon, fromPlace = c(-95.4779683339016, 29.7184856257286), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
rodriguez$name <- "Rodriguez"
stgeorge <- otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
stgeorge$name <- "St. George"
franklin <- otp_isochrone(otpcon, fromPlace = c(-95.2975791420238, 29.7391871459229), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
franklin$name <- "Franklin"
westuniversity <- otp_isochrone(otpcon, fromPlace = c(-95.4352189871682, 29.7156276836808), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
westuniversity$name <- "West University"
briargrove <- otp_isochrone(otpcon, fromPlace = c(-95.4911907488293, 29.7492789636356), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
briargrove$name <- "Briargrove"
cunningham <- otp_isochrone(otpcon, fromPlace = c(-95.4691903870566, 29.7175498450765), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
cunningham$name <- "Cunningham"
kashmeregardens <- otp_isochrone(otpcon, fromPlace = c(-95.3180007793241, 29.8052579465821), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
kashmeregardens$name <- "Kashmere Gardens"
davila <- otp_isochrone(otpcon, fromPlace = c(-95.2843092832056, 29.7141002601876), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
davila$name <- "Davila"
cage <- otp_isochrone(otpcon, fromPlace = c(-95.3335253622882, 29.734852144603), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
cage$name <- "Cage"
ross <- otp_isochrone(otpcon, fromPlace = c(-95.3413741344892, 29.8010269555719), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
ross$name <- "Ross"
sinclair <- otp_isochrone(otpcon, fromPlace = c(-95.42845397905, 29.7937885494746), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
sinclair$name <- "Sinclair"
blackshear <- otp_isochrone(otpcon, fromPlace = c(-95.3621817606426, 29.7295644645081), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
blackshear$name <- "Blackshear"
pleasantville <- otp_isochrone(otpcon, fromPlace = c(-95.2719492742977, 29.7640050064841), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
pleasantville$name <- "Pleasantville"
carillo <- otp_isochrone(otpcon, fromPlace = c(-95.310946424075, 29.7286663480845), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
carillo$name <- "Carillo"
smith <- otp_isochrone(otpcon, fromPlace = c(-95.4616616904167, 29.8393051397641), 
                          mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
smith$name <- "Smith"
scarborough <- otp_isochrone(otpcon, fromPlace = c(-95.3375487420912, 29.8720796989498), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
scarborough$name <- "Scarborough"
tijerina <- otp_isochrone(otpcon, fromPlace = c(-95.3097862274842, 29.7409390671204), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
tijerina$name <- "Tijerina"
helms <- otp_isochrone(otpcon, fromPlace = c(-95.4070161884996, 29.8051561951387), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
helms$name <- "Helms"
porthouston <- otp_isochrone(otpcon, fromPlace = c(-95.2833509570976, 29.7600737863995), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
porthouston$name <- "Port Houston"
crockett <- otp_isochrone(otpcon, fromPlace = c(-95.3796567446341, 29.774044421506), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
crockett$name <- "Crockett"
kennedy <- otp_isochrone(otpcon, fromPlace = c(-95.3939750256933, 29.8348467565282), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
kennedy$name <- "Kennedy"
isaacs <- otp_isochrone(otpcon, fromPlace = c(-95.3182351436396, 29.794797955479), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
isaacs$name <- "Isaacs"
oakforest <- otp_isochrone(otpcon, fromPlace = c(-95.4360353394058, 29.8279021113738), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
oakforest$name <- "Oak Forest"
highlandheights <- otp_isochrone(otpcon, fromPlace = c(-95.4208327208801, 29.8491335755), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
highlandheights$name <- "Highland Heights"
atherton <- otp_isochrone(otpcon, fromPlace = c(-95.3219882933709, 29.7797400906363), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
atherton$name <- "Atherton"
askew <- otp_isochrone(otpcon, fromPlace = c(-95.5773389229144, 29.7522208512209), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
askew$name <- "Askew"
wharton <- otp_isochrone(otpcon, fromPlace = c(-95.3906884477023, 29.7544627178372), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
wharton$name <- "Wharton"
field <- otp_isochrone(otpcon, fromPlace = c(-95.3891973378951, 29.800898291585), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
field$name <- "Field"
lyons <- otp_isochrone(otpcon, fromPlace = c(-95.3665859189985, 29.856249018841), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
lyons$name <- "Lyon"
browning <- otp_isochrone(otpcon, fromPlace = c(-95.3789901350797, 29.797310875744), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
browning$name <- "Browning"
gregorylincoln <- otp_isochrone(otpcon, fromPlace = c(-95.3850475006033, 29.7562654367345), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
gregorylincoln$name <- "Gregory-Lincoln"
marshall <- otp_isochrone(otpcon, fromPlace = c(-95.2977630675878, 29.8982997011356), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
marshall$name <- "Marshall"
oates <- otp_isochrone(otpcon, fromPlace = c(-95.2529636528308, 29.7929533878615), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
oates$name <- "Oates"
stevens <- otp_isochrone(otpcon, fromPlace = c(-95.4461132813987, 29.8272233096243), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
stevens$name <- "Stevens"
moreno <- otp_isochrone(otpcon, fromPlace = c(-95.3901254428171, 29.8752568578185), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
moreno$name <- "Moreno"
benbrook <- otp_isochrone(otpcon, fromPlace = c(-95.4810733432821, 29.826713409923), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
benbrook$name <- "Benbrook"
gallegos <- otp_isochrone(otpcon, fromPlace = c(-95.2933537154773, 29.7334374412449), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
gallegos$name <- "Gallegos"
shadydale <- otp_isochrone(otpcon, fromPlace = c(-95.3114804947605, 29.8494775690593), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
shadydale$name <- "Shadydale"
jphenderson <- otp_isochrone(otpcon, fromPlace = c(-95.3204202704951, 29.7229451289273), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
jphenderson$name <- "Henderson JP"
briscoe <- otp_isochrone(otpcon, fromPlace = c(-95.2983752081754, 29.7304905559507), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
briscoe$name <- "Briscoe"
northline <- otp_isochrone(otpcon, fromPlace = c(-95.3869305899514, 29.8514767974519), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
northline$name <- "Northline"
herrera <- otp_isochrone(otpcon, fromPlace = c(-95.3668884930048, 29.8224421613894), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
herrera$name <- "Herrera"
eliot <- otp_isochrone(otpcon, fromPlace = c(-95.3062865025279, 29.7811833750037), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
eliot$name <- "Eliot"
looscan <- otp_isochrone(otpcon, fromPlace = c(-95.3576505121734, 29.7953349625407), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
looscan$name <- "Looscan"
mcgowen <- otp_isochrone(otpcon, fromPlace = c(-95.2999090448164, 29.8205779212632), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
mcgowen$name <- "McGowen"
burrus <- otp_isochrone(otpcon, fromPlace = c(-95.3904256987588, 29.8172975124517), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
burrus$name <- "Burrus"
emerson <- otp_isochrone(otpcon, fromPlace = c(-95.5339599905737, 29.7265116767701), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
emerson$name <- "Emerson"
scroggins <- otp_isochrone(otpcon, fromPlace = c(-95.2991081521224, 29.7808994635411), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
scroggins$name <- "Scroggins"
poe <- otp_isochrone(otpcon, fromPlace = c(-95.4080125911735, 29.7271990386667), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
poe$name <- "Poe"
roosevelt <- otp_isochrone(otpcon, fromPlace = c(-95.372700509318, 29.8186183992454), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
roosevelt$name <- "Roosevelt"
rmartinez <- otp_isochrone(otpcon, fromPlace = c(-95.2970540808107, 29.7736017583792), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
rmartinez$name <- "R. Martinez"
coop <- otp_isochrone(otpcon, fromPlace = c(-95.3487757678337, 29.8583765258045), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
coop$name <- "Coop"
rpharris <- otp_isochrone(otpcon, fromPlace = c(-95.2213065070223, 29.7690742679742), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
rpharris$name <- "Roland P. Harris"
elmore <- otp_isochrone(otpcon, fromPlace = c(-95.2749470054298, 29.8088784943191), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
elmore$name <- "Elmore"
pineypoint <- otp_isochrone(otpcon, fromPlace = c(-95.5216704580452, 29.7249131484397), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
pineypoint$name <- "Piney Point"
macgregor <- otp_isochrone(otpcon, fromPlace = c(-95.3807619846049, 29.7281557550376), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
macgregor$name <- "MacGregor"
dogan <- otp_isochrone(otpcon, fromPlace = c(-95.3290357496646, 29.785282977023), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
dogan$name <- "Dogan"
bruce <- otp_isochrone(otpcon, fromPlace = c(-95.3422240753208, 29.7669569030046), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
bruce$name <- "Bruce"
riveroaks <- otp_isochrone(otpcon, fromPlace = c(-95.4195314429379, 29.7477086816541), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
riveroaks$name <- "River Oaks"
robinson <- otp_isochrone(otpcon, fromPlace = c(-95.2127348597199, 29.7900603181527), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
robinson$name <- "Robinson"
janowski <- otp_isochrone(otpcon, fromPlace = c(-95.3712159599219, 29.8302702004534), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
janowski$name <- "Janowski"
garcia <- otp_isochrone(otpcon, fromPlace = c(-95.3483845098303, 29.8492670640573), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
garcia$name <- "Garcia"
lockhart <- otp_isochrone(otpcon, fromPlace = c(-95.3661635859358, 29.7178662243048), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
lockhart$name <- "Lockhart"
osborne <- otp_isochrone(otpcon, fromPlace = c(-95.4163796781469, 29.8757822175177), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
osborne$name <- "Osborne"
pilgrim <- otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
pilgrim$name <- "Pilgrim"
reagan <- otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034), 
                        mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
reagan$name <- "Reagan"
travis <- otp_isochrone(otpcon, fromPlace = c(-95.3745532440293, 29.7890298734142), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
travis$name <- "Travis"
love <- otp_isochrone(otpcon, fromPlace = c(-95.4089218427404, 29.7937422436856), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
love$name <- "Love"
wainwright <- otp_isochrone(otpcon, fromPlace = c(-95.4690833145607, 29.8231569069759), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
wainwright$name <- "Wainwright"
cook <- otp_isochrone(otpcon, fromPlace = c(-95.3195175276709, 29.8240361841286), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
cook$name <- "Cook"
henderson <- otp_isochrone(otpcon, fromPlace = c(-95.3213905503091, 29.7692088765697), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
henderson$name <- "Henderson NQ"
pugh <- otp_isochrone(otpcon, fromPlace = c(-95.3068551426651, 29.7682966632075), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
pugh$name <- "Pugh"
berry <- otp_isochrone(otpcon, fromPlace = c(-95.3503090193495, 29.8367204981972), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
berry$name <- "Berry"
ketelson <- otp_isochrone(otpcon, fromPlace = c(-95.3636411176229, 29.7807384148368), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
ketelson$name <- "Ketelson"
sherman <- otp_isochrone(otpcon, fromPlace = c(-95.3524953807112, 29.7794092687264), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
sherman$name <- "Sherman"
roderickrpaige <- otp_isochrone(otpcon, fromPlace = c(-95.33863596952, 29.8271607354241), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
roderickrpaige$name <- "Paige"
martinez <- otp_isochrone(otpcon, fromPlace = c(-95.3591770111663, 29.7904615176006), 
                           mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
martinez$name <- "Martinez"
neff <- otp_isochrone(otpcon, fromPlace = c(-95.5318014654878, 29.7002506228244), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
daily <- otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034), 
                       mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
daily$name  <- "Daily"

neff$name <- "Neff"
twain <- otp_isochrone(otpcon, fromPlace = c(-95.4377403455408, 29.6985970665278), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
twain$name <- "Twain"
fondren <- otp_isochrone(otpcon, fromPlace = c(-95.4983892831844, 29.6281616672788), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
fondren$name <- "Fondren"
thompson <- otp_isochrone(otpcon, fromPlace = c(-95.3630257741897, 29.7055569096654), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
thompson$name <- "Thompson"
mading <- otp_isochrone(otpcon, fromPlace = c(-95.3274325837129, 29.6595002066014), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
mading$name <- "Mading"
young <- otp_isochrone(otpcon, fromPlace = c(-95.3705805494992, 29.6697873581584), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
young$name <- "Young"
milne <- otp_isochrone(otpcon, fromPlace = c(-95.5155393708139, 29.6612004153328), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
milne$name <- "Milne"
codwell <- otp_isochrone(otpcon, fromPlace = c(-95.3454914084816, 29.6341181907014), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
codwell$name <- "Codwell"
kelso <- otp_isochrone(otpcon, fromPlace = c(-95.3338648297985, 29.682008748293), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
kelso$name <- "Kelso"
brookline <- otp_isochrone(otpcon, fromPlace = c(-95.3148010477487, 29.6983048818679), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
brookline$name <- "Brookline"
raydaily <- otp_isochrone(otpcon, fromPlace = c(-95.6155637908058, 29.7535241832467), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
raydaily$name <- "Ray Daily"
deanda <- otp_isochrone(otpcon, fromPlace = c(-95.2794506350266, 29.6248452463028), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
deanda$name <- "DeAnda"
lewis <- otp_isochrone(otpcon, fromPlace = c(-95.2826446311274, 29.6656125440621), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
lewis$name <- "Lewis"
herod <- otp_isochrone(otpcon, fromPlace = c(-95.4878915591545, 29.6834969000504), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
herod$name <- "Herod"
alcott <- otp_isochrone(otpcon, fromPlace = c(-95.3296375525859, 29.667490606147), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
alcott$name <- "Alcott"
gardenvillas <- otp_isochrone(otpcon, fromPlace = c(-95.3040391100433, 29.6613722657401), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
gardenvillas$name <- "Garden Villas"
lovett <- otp_isochrone(otpcon, fromPlace = c(-95.4690972798077, 29.6830908115424), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
lovett$name <- "Lovett"
law <- otp_isochrone(otpcon, fromPlace = c(-95.3643018073766, 29.6277015424835), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
law$name <- "Law"
gross <- otp_isochrone(otpcon, fromPlace = c(-95.527753258014, 29.639268448191), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
gross$name <- "Gross"
peck <- otp_isochrone(otpcon, fromPlace = c(-95.3342906743019, 29.7064046490361), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
peck$name <- "Peck"
parkplace <- otp_isochrone(otpcon, fromPlace = c(-95.2737853333731, 29.688800284984), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
parkplace$name <- "Park Place"
southmayd <- otp_isochrone(otpcon, fromPlace = c(-95.2888674248659, 29.710110133712), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
southmayd$name <- "Southmayd"
bell <- otp_isochrone(otpcon, fromPlace = c(-95.5490140435891, 29.6437936746034), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
bell$name <- "Bell"
reynolds <- otp_isochrone(otpcon, fromPlace = c(-95.3751775264176, 29.6516440497087), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
reynolds$name <- "Reynolds"
windsorvillage <- otp_isochrone(otpcon, fromPlace = c(-95.4711907272968, 29.6175570233885), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
windsorvillage$name <- "Windsor Village"
valleywest <- otp_isochrone(otpcon, fromPlace = c(-95.5267257617345, 29.6635857622773), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
valleywest$name <- "Valley West"
hobby <- otp_isochrone(otpcon, fromPlace = c(-95.4372238012853, 29.6353652034036), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
hobby$name <- "Hobby"
hartsfield <- otp_isochrone(otpcon, fromPlace = c(-95.343003253344, 29.6942305681583), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
hartsfield$name <- "Hartsfield"
sanchez <- otp_isochrone(otpcon, fromPlace = c(-95.2844813428672, 29.7027018350688), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
sanchez$name <- "Sanchez"
golfcrest <- otp_isochrone(otpcon, fromPlace = c(-95.2952435228517, 29.68895059784), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
golfcrest$name <- "Golfcrest"
longfellow <- otp_isochrone(otpcon, fromPlace = c(-95.4325424719515, 29.6869793169525), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
longfellow$name <- "Longfellow"
red <- otp_isochrone(otpcon, fromPlace = c(-95.4550355013232, 29.6665619503333), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
red$name <- "Red"
bastian <- otp_isochrone(otpcon, fromPlace = c(-95.3498800546384, 29.6678764647414), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
bastian$name <- "Bastian"
crespo <- otp_isochrone(otpcon, fromPlace = c(-95.2879048783548, 29.6993432782904), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
crespo$name <- "Crespo"
bonham <- otp_isochrone(otpcon, fromPlace = c(-95.5195416542261, 29.6876239037611), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
bonham$name <- "Bonham"
anderson <- otp_isochrone(otpcon, fromPlace = c(-95.4874945522717, 29.6484193194829), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
anderson$name <- "Anderson"
condit <- otp_isochrone(otpcon, fromPlace = c(-95.4672405854874, 29.7028920038785), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
condit$name <- "Condit"
elrod <- otp_isochrone(otpcon, fromPlace = c(-95.5015409919633, 29.6714891198385), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
elrod$name <- "Elrod"
shearn <- otp_isochrone(otpcon, fromPlace = c(-95.4408659943328, 29.6774699913992), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
shearn$name <- "Shearn"
mitchell <- otp_isochrone(otpcon, fromPlace = c(-95.2759010951547, 29.6076307934148), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
mitchell$name <- "Mitchell"
frost <- otp_isochrone(otpcon, fromPlace = c(-95.3367639766319, 29.6206659663223), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
frost$name <- "Frost"
foster <- otp_isochrone(otpcon, fromPlace = c(-95.3604159053805, 29.6924161861956), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
foster$name <- "Foster"
parker <- otp_isochrone(otpcon, fromPlace = c(-95.4830693915208, 29.6656335622772), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
parker$name <- "Parker"
rucker <- otp_isochrone(otpcon, fromPlace = c(-95.2419372242408, 29.6937240962718), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
rucker$name <- "Rucker"
ashford <- otp_isochrone(otpcon, fromPlace = c(-95.5977733826487, 29.7470427195492), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
ashford$name <- "Ashford"
almeda <- otp_isochrone(otpcon, fromPlace = c(-95.4183915170341, 29.6000531413948), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
almeda$name <- "Almeda"
gregg <- otp_isochrone(otpcon, fromPlace = c(-95.3087001348675, 29.6735377875108), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
gregg$name <- "Gregg"
seguin <- otp_isochrone(otpcon, fromPlace = c(-95.3061984619901, 29.678754054483), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
seguin$name <- "Seguin"
bonner <- otp_isochrone(otpcon, fromPlace = c(-95.2599641354143, 29.6766482201247), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
bonner$name <- "Bonner"
foerster <- otp_isochrone(otpcon, fromPlace = c(-95.498509034359, 29.6394252759728), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
foerster$name <- "Foerster"
grissom <- otp_isochrone(otpcon, fromPlace = c(-95.4552141362474, 29.6206228353323), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
grissom$name <- "Grissom"
montgomery <- otp_isochrone(otpcon, fromPlace = c(-95.4366108474982, 29.6203405080538), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
montgomery$name <- "Montgomery"
tinsley <- otp_isochrone(otpcon, fromPlace = c(-95.4996695508896, 29.6588229377742), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
tinsley$name <- "Tinsley"
horn <- otp_isochrone(otpcon, fromPlace = c(-95.454268450125, 29.6935459156749), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
horn$name <- "Horn"
shadowbriar <- otp_isochrone(otpcon, fromPlace = c(-95.5989055373763, 29.7374592612143), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
shadowbriar$name <- "Shadowbriar"
whidby <- otp_isochrone(otpcon, fromPlace = c(-95.3751129432108, 29.6821219139344), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
whidby$name <- "Whidby"
hines <-  otp_isochrone(otpcon, fromPlace = c(-95.3604283,29.7570034), 
                        mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
hines$name <- "Hines-Caldwell"
cornelius <- otp_isochrone(otpcon, fromPlace = c(-95.2941348749098, 29.6780848030339), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
cornelius$name <- "Cornelius"
edwhite <- otp_isochrone(otpcon, fromPlace = c(-95.5431418772178, 29.6959959255029), 
                      mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
edwhite$name <- "Ed White"
patterson <- otp_isochrone(otpcon, fromPlace = c(-95.2422677646535, 29.6823786933878), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
patterson$name <- "Patterson"
sutton <- otp_isochrone(otpcon, fromPlace = c(-95.5022688764514, 29.6975854825615), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
sutton$name <- "Sutton"
petersen <- otp_isochrone(otpcon, fromPlace = c(-95.4250357098875, 29.6179514145333), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
petersen$name <- "Petersen"
braeburn <- otp_isochrone(otpcon, fromPlace = c(-95.4878212778189, 29.6955669167768), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
braeburn$name <- "Braeburn"
mcnamara <- otp_isochrone(otpcon, fromPlace = c(-95.5045131060551, 29.6843810055235), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
mcnamara$name <- "McNamara"
bush <- otp_isochrone(otpcon, fromPlace = c(-95.6325834118217, 29.7602213781054), 
                         mode = c("WALK", "BICYCLE", "TRANSIT"), cutoffSec = 1800)
bush$name <- "Bush"


schoolisochrones <- rbind(alcott,almeda,anderson,ashford,askew,atherton,barrick,bastian,bell,
                          benavidez,benbrook,berry,blackshear,bonham,bonner,braeburn,briargrove,
                          briscoe,brookline,browning,bruce,burbank,burnet,burrus,bush,cage,
                          carillo,codwell,condit,cook,coop,cornelius,crespo,
                          crockett,cunningham,daily,davila,deanda,dechaumes,dezavala,dogan,durham,durkee,
                          edwhite,eliot,elmore,elrod,emerson,field,foerster,fondren,foster,franklin,
                          frost,gallegos,garcia,gardenoaks,gardenvillas,golfcrest,gregg,gregorylincoln,grissom,
                          gross,hartsfield,harvard,helms,henderson,herod,herrera,highlandheights,
                          hilliard,hines,hobby,horn,isaacs,janowski,jefferson,jphenderson,jrharris,
                          kashmeregardens,kelso,kennedy,ketelson,kolter,lantrip,law,lewis,lockhart,
                          longfellow,looscan,love,lovett,lyons,macgregor,mading,markwhite,marshall,
                          martinez,mcgowen,mcnamara,memorial,milne,mitchell,montgomery,moreno,neff,northline,
                          oakforest,oates,osborne,parker,parkplace,patterson,peck,petersen,pilgrim,pineypoint,
                          pleasantville,poe,porthouston,pugh,raydaily,reagan,red,reynolds,riveroaks,rmartinez,
                          roberts,robinson,roderickrpaige,rodriguez,roosevelt,ross,rpharris,rucker,
                          sanchez,scarborough,scroggins,seguin,shadowbriar,shadydale,shearn,sherman,
                          sinclair,smith,southmayd,stevens,stgeorge,sutton,thompson,tijerina,tinsley,travis,
                          twain,valleywest,wainwright,walnutbend,wesley,westuniversity,wharton,whidby,whittier,
                          wilson,windsorvillage,woodson,young)
schoolisochrones <- schoolisochrones[-c(140),]

st_write(schoolisochrones, "isochrones.shp")

