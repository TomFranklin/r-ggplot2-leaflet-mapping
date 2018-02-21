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
countries_europe <- spTransform(countries_europe, CRS("+proj=longlat +ellps=WGS84"))

# Fortify shapefile so we can plot it 
fortify(countries_europe)

theme_map <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="white", colour=NA),
      legend.key=element_rect(colour="white"),
      legend.key.size=unit(1.2, "lines"),
      legend.position="right",
      legend.text=element_text(size=rel(0)),
      legend.title=element_text(size=rel(0), face="bold", hjust=0),
      panel.background=element_rect(fill = "grey90", colour = "white"),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.margin=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=rel(0)),
      strip.background=element_rect(fill="white", colour="white"),
      strip.text.x=element_text(size=rel(0)),
      strip.text.y=element_text(size=rel(0), angle=-90) 
    )   
}




#### 
# 3. Mapping ----
# Dark Green "#00504B" 
# Light Green "#00A19A"

#### 
# Plot map of Europe on Earth
ggplot() +
  geom_polygon(data = countries_europe, aes(fill = order, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) + 
  coord_equal() + 
  theme_minimal()
 
####
# Map of Europe without colouring

 


#### 
# Map of Europe inc Iceland ----
tic()
map_europe_inc_iceland <- ggplot() +
  geom_polygon(data = countries_europe, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               color = "white",
               fill = "navy", 
               size = 0.05) + 
  coord_equal(xlim = c(-23, 32),ylim = c(30, 72)) + 
  theme_map()

map_europe_inc_iceland
toc()

#### 
# Map of Europe exc Iceland ----
tic()
map_europe_exc_iceland <- ggplot() +
  geom_polygon(data = countries_europe, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               color = "#00A19A",
               fill = "#00504B") + 
  coord_equal(xlim = c(-11, 30),ylim = c(35, 70)) + 
  theme_map()
toc()

####
# Map of UK 
tic()
uk_map <- ggplot() +
  geom_polygon(data = countries_europe, 
               aes(x = long, 
               y = lat, 
              group = group),
              color = "black",
              fill = "white") + 
  coord_equal(xlim = c(-10, 3),ylim = c(49, 62)) + 
  theme_map()
toc()


#### 
# 4. Print maps

png(filename="maps_produced_ggplot/map_europe_inc_iceland_2.png", 
    units="in", 
    width=8, 
    height=8, 
    pointsize=12, 
    res=300)
map_europe_inc_iceland
dev.off()

png(filename="maps_produced_ggplot/map_europe_exc_iceland_2.png", 
    units="in", 
    width=8, 
    height=8, 
    pointsize=12, 
    res=300)
map_europe_exc_iceland
dev.off()

png(filename="maps_produced_ggplot/map_uk_2.png", 
    units="in", 
    width=8, 
    height=8, 
    pointsize=12, 
    res=300)
map_uk
dev.off()

# Annex ----
# http://www.naturalearthdata.com/downloads/10m-cultural-vectors/ Sovereignty shapefiles
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf  Transforming data


#### 
# Map of Europe exc Iceland ----
tic()
ggplot() +
  geom_polygon(data = countries_europe, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               color = "black",
               fill = "blue") + 
  coord_equal(xlim = c(-11, 30),ylim = c(35, 70)) + 
  theme_nogrid() + 
  labs(caption = "Source: Manchester Metrics") 
toc()


theme_mcr_metrics_map <- function(){
}



theme_mcr_metrics_map <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text = element_text(colour = NULL),
      axis.title.x = element_text(colour = NULL, size=rel(3)),
      axis.title.y = element_text(colour = NULL, angle=45),
      panel.background = element_rect(fill="grey"),
      panel.grid.minor.y = element_line(),
      panel.grid.major = element_line(colour = NULL),
      plot.background = element_rect(fill="grey")
    )   
}

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_void(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.grid = element_blank()
    )   
}


# Map of UK 
tic()
map_uk <- ggplot() +
  geom_polygon(data = countries_europe, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               color = "black",
               fill = "white") + 
  coord_equal(xlim = c(-10, 3),ylim = c(49, 62)) + 
  theme_map()
toc()
map_uk



#### 
# Map of Europe exc Iceland good----
tic()
test <- ggplot() +
  geom_polygon(data = countries_europe, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               color = "white",
               fill = "#00A19A", 
               size = 0.05) + 
  coord_equal(xlim = c(-11, 30),ylim = c(35, 70)) + 
  theme_map() + 
  labs(caption = "Source: Manchester Metrics")
test
toc()
