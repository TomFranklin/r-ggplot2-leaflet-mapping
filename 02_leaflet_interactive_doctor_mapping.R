# TF 09/01/17
# Mapping the World with leaflet and open source natural earth data

# 
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
library(htmlwidgets)
library(tidyr)
####
# 2. Load data and create shapefiles for maps ----

# rawData <- read_csv(file = "Data/SFR14_2017_UD_LA_characteristics_6_half_terms.csv")
countries_shapefile <- shapefile("Shapefiles/ne_10m_admin_0_map_units.shp")
# plot(countries_shapefile)

# One way to filter countries by starting letter of country!

countries_uk = subset(countries_shapefile, SUBUNIT %like% "England" | 
                        SUBUNIT%like% "Wales" |
                        SUBUNIT %like% "Scotland" |
                        SUBUNIT %like% "Northern Ireland")

countries_uk <- spTransform(countries_uk, CRS("+proj=longlat +ellps=WGS84"))

doctor_data <- read.csv("data/doctors.csv")

doctor_data %>% 
  dplyr::filter(Year == 2017) %>% 
  dplyr::select(Country, Gender, Year, Number) %>%
  tidyr::spread(Gender, Number) -> doctor_data

doctor_data$Male <- as.numeric(sub(",", "", doctor_data$Male, fixed = TRUE))
doctor_data$Female <- as.numeric(sub(",", "", doctor_data$Female, fixed = TRUE))

doctor_data %>%
  mutate(majority_gender = ifelse(Male > Female, "Majority of doctors are male", 
                           ifelse(Female > Male, "Majority of doctors are female", "Gender Equality")))  %>%
  mutate(total = Male + Female) %>%
  mutate(Male_prop = (Male / total)*100) %>%
  mutate(Female_prop = (Female / total)*100) -> doctor_data

doctor_data$Male_prop <- format(round(doctor_data$Male_prop, 2), nsmall = 2)
doctor_data$Female_prop <- format(round(doctor_data$Female_prop, 2), nsmall = 2)

doctor_data$Country <- as.character(doctor_data$Country)

doctor_data <- droplevels(doctor_data)

doctor_data$majority_gender <- as.factor(doctor_data$majority_gender)

data_for_mapping <- sp::merge(countries_uk,
                          doctor_data,
                          by.x = 'SUBUNIT',
                          by.y = 'Country',
                          duplicateGeoms = TRUE)



map_pal = colorFactor(c('purple', '#4169e1'), data_for_mapping$majority_gender)


map_label <- sprintf("<strong>%g</strong><br/>Male
                     <strong>%g</strong><br/>Female 
                     <strong>%s</strong><br/>Country",
                     data_for_mapping$Male, 
                     data_for_mapping$Female, 
                     data_for_mapping$Country) %>%
  lapply(htmltools::HTML)






hoverText <- sprintf("<div style='font-size:12px;width:200px;float:left'>
            <span style='font-size:18px;font-weight:bold'>%s</span><br/> 
            <div style='width:95%%'>
              <span style='float:left'>Male</span>
                     <span style='float:right'>Female</span>
                     <br/>
                     <span style='color:black;float:left'>%s%%</span>
                     <span style='color:black;float:right'>%s%%</span><br clear='all'/>
                     <span style='background:#D4DCF7;width:%s%%;float:left'>&nbsp;</span>
                     <span style='background:#E7CCFC;width:%s%%;float:right'>&nbsp;</span>
                     </div>
                     <br/><span style='font-size:10px'>%s</span>
                     </div>",
                      data_for_mapping$SUBUNIT, 
                      data_for_mapping$Male_prop, data_for_mapping$Female_prop,
                      data_for_mapping$Male_prop, data_for_mapping$Female_prop,
                     data_for_mapping$majority_gender) %>%
  lapply(htmltools::HTML)




leaflet(data_for_mapping,
  options=leafletOptions(attributionControl = FALSE, 
  dragging = FALSE, zoomControl = FALSE, minZoom = 5.8, maxZoom = 5.8)) %>%
  addPolygons(fillColor=~map_pal(data_for_mapping$majority_gender),
              weight = 1,
              label = ~hoverText,
              color = "grey",
              labelOptions = labelOptions(
                offset = c(-100,-140),
                #direction='bottom',
                textOnly = T,
                style=list(
                  'background'='rgba(255,255,255,0.95)',
                  'border-color' = 'rgba(0,0,0,1)',
                  'border-radius' = '4px',
                  'border-style' = 'solid',
                  'border-width' = '4px')),
              highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)) %>%
  setMaxBounds(lat1 = 60, lng1 = 8.82, lat2 = 50, lng2 = -16) %>%
  htmlwidgets::onRender(
    "function(el, t) {
    var myMap = this;
    // get rid of the ugly grey background
    myMap._container.style['background'] = '#ffffff';
    }") 
    
