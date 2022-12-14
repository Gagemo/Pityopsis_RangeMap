#########################   Pityopsis Range Map     ############################
#########################  University of Florida  ##############################
#########################       Gage LaPierre     ##############################
#########################          2022           ##############################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

######################### Clears Environment & History  ########################

rm(list=ls(all=TRUE))
cat("\014") 

#########################     Installs Packages   ##############################

list.of.packages <- c("tidyverse", "sp", "rgdal", "rgdal", "rgeos", "raster", 
                      "rgbif", "tmap", "rinat", "leaflet", "conflicted", "sf", 
                      "rangemap", "mapview", "htmlwidgets")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################     Loads Packages     ##############################

library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(tmap)
library(rinat)
library(leaflet)
library(conflicted)
library(sf)
library(rangemap)
library(rgbif)
library(mapview)
library(htmlwidgets)

################## Prevents Conflict with Functions   ##########################

conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("count", "dplyr", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("arrange", "dplyr", quiet = TRUE)

############ Loads Data & Transforms Map Data to Correct Format ################

FL_sf <- st_read("geojson-fl-counties-fips.json", quiet = TRUE) %>% 
  st_transform(4326)

################# Creates Bounding Box to Pull iNat Data  ######################

FL_bb <- FL_sf %>% 
  st_bbox()

################ Downloads iNat data for each species ##########################
################ Checks & prevents re download #################################
# P. apsera
search_P.aspera_fn <- "P.aspera_FL.Rdata"
if (file.exists(search_P.aspera_fn)) {
  load(search_P.aspera_fn)
} else {  
  inat_P.aspera_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                   taxon_name = "Pityopsis aspera",
                                   year = NULL,
                                   month = NULL,
                                   maxresults = 1000)
  save(inat_P.aspera_df, file = search_P.aspera_fn)
}
# P.aequilifolia 
search_P.aequilifolia_fn <- "P.aequilifolia_FL.Rdata"
if (file.exists(search_P.aequilifolia_fn)) {
  load(search_P.aequilifolia_fn)
} else {  
  inat_P.aequilifolia_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                         taxon_name = "Pityopsis aequilifolia",
                                         year = NULL,
                                         month = NULL,
                                         maxresults = 1000)
  save(inat_P.aequilifolia_df, file = search_P.aequilifolia_fn)
}
# P.flexuosa 
search_P.flexuosa_fn <- "P.flexuosa_FL.Rdata"
if (file.exists(search_P.flexuosa_fn)) {
  load(search_P.flexuosa_fn)
} else {  
  inat_P.flexuosa_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                     taxon_name = "Pityopsis flexuosa",
                                     year = NULL,
                                     month = NULL,
                                     maxresults = 1000)
  save(inat_P.flexuosa_df, file = search_P.flexuosa_fn)
}
# P.latifolia  
search_P.latifolia_fn <- "P.latifolia_FL.Rdata"
if (file.exists(search_P.latifolia_fn)) {
  load(search_P.latifolia_fn)
} else {  
  inat_P.latifolia_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                      taxon_name = "Pityopsis graminifolia latifolia",
                                      year = NULL,
                                      month = NULL,
                                      maxresults = 1000)
  save(inat_P.latifolia_df, file = search_P.latifolia_fn)
}
# P.graminifolia   
search_P.graminifolia_fn <- "P.graminifolia_FL.Rdata"
if (file.exists(search_P.graminifolia_fn)) {
  load(search_P.graminifolia_fn)
} else {  
  inat_P.graminifolia_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                         taxon_name = "Pityopsis graminifolia",
                                         year = NULL,
                                         month = NULL,
                                         maxresults = 1000)
  save(inat_P.graminifolia_df, file = search_P.graminifolia_fn)
}
# P.oligantha
search_P.oligantha_fn <- "P.oligantha_FL.Rdata"
if (file.exists(search_P.oligantha_fn)) {
  load(search_P.oligantha_fn)
} else {  
  inat_P.oligantha_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                      taxon_name = "Pityopsis oligantha",
                                      year = NULL,
                                      month = NULL,
                                      maxresults = 1000)
  save(inat_P.oligantha_df, file = search_P.oligantha_fn)
}
# P.tracyi
search_P.tracyi_fn <- "P.tracyi_FL.Rdata"
if (file.exists(search_P.tracyi_fn)) {
  load(search_P.tracyi_fn)
} else {  
  inat_P.tracyi_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                   taxon_name = "Pityopsis tracyi",
                                   year = NULL,
                                   month = NULL,
                                   maxresults = 1000)
  save(inat_P.tracyi_df, file = search_P.tracyi_fn)
}

################ Saves iNat Data into Data Frame  ##############################
################ Selects Key Columns & Combines with Map Data ##################

inat_P.aspera_pcsp_popup_sf <- inat_P.aspera_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_P.aequilifolia_pcsp_popup_sf <- inat_P.aequilifolia_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_P.flexuosa_pcsp_popup_sf <- inat_P.flexuosa_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_P.latifolia_pcsp_popup_sf <- inat_P.latifolia_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_P.graminifolia_pcsp_popup_sf <- inat_P.graminifolia_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_P.oligantha_pcsp_popup_sf <- inat_P.oligantha_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

inat_P.tracyi_pcsp_popup_sf <- inat_P.tracyi_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

############################# Creates Map  #####################################

htmltools::p("Native Pityopsis in Florida", 
             style = "font-weight:bold; font-size:110%;")

Pityopsis = leaflet(FL_sf, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("GeoportailFrance.orthos") %>% 
  addPolygons(fill = TRUE, fillColor = "forestgreen",color = "black", 
              weight = 1, fillOpacity = 0.4) %>% 
  addCircleMarkers(data = inat_P.aspera_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#E69F00", opacity = 1,
                   fillOpacity = 1, color = "black")%>% 
  addCircleMarkers(data = inat_P.aequilifolia_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#56B4E9", opacity = 1,
                   fillOpacity = 1, color = "black",)%>% 
  addCircleMarkers(data = inat_P.flexuosa_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#009E73", opacity = 1,
                   fillOpacity = 1, color = "black")%>% 
  addCircleMarkers(data = inat_P.latifolia_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#D55E00", opacity = 1,
                   fillOpacity = 1, color = "black")%>% 
  addCircleMarkers(data = inat_P.graminifolia_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#F0E442", opacity = 1,
                   fillOpacity = 1, color = "black",)%>% 
  addCircleMarkers(data = inat_P.oligantha_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#0072B2", opacity = 1,
                   fillOpacity = 1, color = "black")%>% 
  addCircleMarkers(data = inat_P.tracyi_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#CC79A7", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addLegend(opacity = 1, position = 'bottomleft', 
            colors = c("#E69F00", "#56B4E9", "#009E73", 
                       "#D55E00", "#F0E442", "#0072B2", "#CC79A7"),
            labels = c("P. aspera", "P. aequilifolia", "P. flexuosa", "P. latifolia", 
                       "P. graminifolia", "P. oligantha", "P. tracyi"),
            title = "Research Grade iNat Observations <br> of Native Pityopsis spp.")
Pityopsis
saveWidget(widget = Pityopsis,
           file = "Pityopsis.html",
           selfcontained = TRUE)
