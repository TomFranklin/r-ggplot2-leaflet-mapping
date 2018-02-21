# TF 09/01/17
# Mapping the World with ggplot and open source natural earth data

####
# Psuedocode ----
# 1. Load libraries
# 2. Load data and map
# 3. Clean data and map
# 4. Join data to map
# Annex
####

####
# 1. Load libraries ----
library(leaflet)
library(geojsonio)
library(rgdal)
library(sp)
library(dplyr)
library(plyr)
library(data.table)
library(RColorBrewer)
library(raster)
library(ggplot2)
library(rgeos)
library(readr)
library(mapproj)
library(tictoc)
library(ggmap)
library(maps)
library(mapdata)
library(ggthemes)
library(htmltools)
####
# 2. Load data and create shapefiles for maps ----

# rawData <- read_csv(file = "Data/SFR14_2017_UD_LA_characteristics_6_half_terms.csv")
countries_shapefile <- shapefile("shapefiles/ne_10m_admin_0_sovereignty.shp")
# plot(countries_shapefile)

# One way to filter countries by starting letter of country!

#countries_beginning_with_e = subset(countries_shapefile, SOVEREIGNT %like% "E")
#countries_beginning_with_u = subset(countries_shapefile, SOVEREIGNT %like% "U")
#countries_beginning_with_f = subset(countries_shapefile, SOVEREIGNT %like% "F")

#countries_beginning_with_f_u = subset(countries_shapefile, SOVEREIGNT %like% "F" | 
#                                                           SOVEREIGNT %like% "U" )

# Filter countries by Europe region
countries_europe = subset(countries_shapefile, REGION_WB %like% "Europe")
unique(countries_europe$SOVEREIGNT)
countries_europe = subset(countries_shapefile, 
                            SOVEREIGNT %like% "Ireland" |
                            SOVEREIGNT %like% "United Kingdom" |
                            SOVEREIGNT %like% "Spain" |
                            SOVEREIGNT %like% "France" |
                            SOVEREIGNT %like% "Iceland" |
                            SOVEREIGNT %like% "Poland" |
                            SOVEREIGNT %like% "Denmark" |
                            SOVEREIGNT %like% "Norway" |
                            SOVEREIGNT %like% "Croatia" |
                            SOVEREIGNT %like% "Germany" |
                            SOVEREIGNT %like% "Netherlands" |
                            SOVEREIGNT %like% "Sweden" |
                            SOVEREIGNT %like% "Slovakia" |
                            SOVEREIGNT %like% "Portugal" |
                            SOVEREIGNT %like% "Italy" |
                            SOVEREIGNT %like% "Greece" |
                            SOVEREIGNT %like% "Slovenia" |
                            SOVEREIGNT %like% "Austria" |
                            SOVEREIGNT %like% "Finland" |
                            SOVEREIGNT %like% "Luxembourg" |
                            SOVEREIGNT %like% "Hungary" |
                            SOVEREIGNT %like% "Switzerland" |
                            SOVEREIGNT %like% "Romania" |
                            SOVEREIGNT %like% "Czechia" |
                            SOVEREIGNT %like% "Estonia" |
                            SOVEREIGNT %like% "Latvia" |
                            SOVEREIGNT %like% "Bulgaria" |
                            SOVEREIGNT %like% "Lithuania" |
                            SOVEREIGNT %like% "Belgium")

unique(countries_europe$SOVEREIGNT)                            

countries_europe <- spTransform(countries_europe, CRS("+proj=longlat +ellps=WGS84"))

# Fortify shapefile so we can plot it 
countries_europe <- spTransform(countries_europe, CRS("+proj=longlat +ellps=WGS84"))


leaflet(countries_europe, 
        options=leafletOptions(attributionControl = FALSE,minZoom = 4, maxZoom = 6)) %>%
  addPolygons(fillColor = "purple", color = "purple", weight = 0.8)  %>%
  htmlwidgets::onRender(
    "function(el, t) {
    var myMap = this;
    // get rid of the ugly grey background
    myMap._container.style['background'] = '#ffffff';
    }") 
    

