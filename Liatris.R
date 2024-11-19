################################################################################
################################################################################
#########################    Liatris Range Map    ##############################
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
# L. aspera
search_L.aspera_fn <- "L.aspera_FL.Rdata"
if (file.exists(search_L.aspera_fn)) {
  load(search_L.aspera_fn)
} else {  
  inat_L.aspera_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                   taxon_name = "Liatris aspera",
                                   year = NULL,
                                   month = NULL,
                                   quality_grade = "research",
                                   maxresults = 1000)
  save(inat_L.aspera_df, file = search_L.aspera_fn)
}

# L. chapmanii
search_L.chapmanii_fn <- "L.chapmanii_FL.Rdata"
if (file.exists(search_L.chapmanii_fn)) {
  load(search_L.chapmanii_fn)
} else {  
  inat_L.chapmanii_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                      taxon_name = "Liatris chapmanii",
                                      year = NULL,
                                      month = NULL,
                                      quality_grade = "research",
                                      maxresults = 1000)
  save(inat_L.chapmanii_df, file = search_L.chapmanii_fn)
}

# L. elegantula
search_L.elegantula_fn <- "L.elegantula_FL.Rdata"
if (file.exists(search_L.elegantula_fn)) {
  load(search_L.elegantula_fn)
} else {  
  inat_L.elegantula_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                       taxon_name = "Liatris elegantula",
                                       year = NULL,
                                       month = NULL,
                                       quality_grade = "research",
                                       maxresults = 1000)
  save(inat_L.elegantula_df, file = search_L.elegantula_fn)
}

# L. garberi
search_L.garberi_fn <- "L.garberi_FL.Rdata"
if (file.exists(search_L.garberi_fn)) {
  load(search_L.garberi_fn)
} else {  
  inat_L.garberi_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                    taxon_name = "Liatris garberi",
                                    year = NULL,
                                    month = NULL,
                                    quality_grade = "research",
                                    maxresults = 1000)
  save(inat_L.garberi_df, file = search_L.garberi_fn)
}

# L. gholsonii
search_L.gholsonii_fn <- "L.gholsonii_FL.Rdata"
if (file.exists(search_L.gholsonii_fn)) {
  load(search_L.gholsonii_fn)
} else {  
  inat_L.gholsonii_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                      taxon_name = "Liatris gholsonii",
                                      year = NULL,
                                      month = NULL,
                                      quality_grade = "research",
                                      maxresults = 1000)
  save(inat_L.gholsonii_df, file = search_L.gholsonii_fn)
}

# L. laevigata
search_L.laevigata_fn <- "L.laevigata_FL.Rdata"
if (file.exists(search_L.laevigata_fn)) {
  load(search_L.laevigata_fn)
} else {  
  inat_L.laevigata_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                      taxon_name = "Liatris laevigata",
                                      year = NULL,
                                      month = NULL,
                                      quality_grade = "research",
                                      maxresults = 1000)
  save(inat_L.laevigata_df, file = search_L.laevigata_fn)
}

# L. ohlingerae
search_L.ohlingerae_fn <- "L.ohlingerae_FL.Rdata"
if (file.exists(search_L.ohlingerae_fn)) {
  load(search_L.ohlingerae_fn)
} else {  
  inat_L.ohlingerae_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                       taxon_name = "Liatris ohlingerae",
                                       year = NULL,
                                       month = NULL,
                                       quality_grade = "research",
                                       maxresults = 1000)
  save(inat_L.ohlingerae_df, file = search_L.ohlingerae_fn)
}

# L. patens
search_L.patens_fn <- "L.patens_FL.Rdata"
if (file.exists(search_L.patens_fn)) {
  load(search_L.patens_fn)
} else {  
  inat_L.patens_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                   taxon_name = "Liatris patens",
                                   year = NULL,
                                   month = NULL,
                                   quality_grade = "research",
                                   maxresults = 1000)
  save(inat_L.patens_df, file = search_L.patens_fn)
}

# L. pauciflora
search_L.pauciflora_fn <- "L.pauciflora_FL.Rdata"
if (file.exists(search_L.pauciflora_fn)) {
  load(search_L.pauciflora_fn)
} else {  
  inat_L.pauciflora_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                       taxon_name = "Liatris pauciflora",
                                       year = NULL,
                                       month = NULL,
                                       quality_grade = "research",
                                       maxresults = 1000)
  save(inat_L.pauciflora_df, file = search_L.pauciflora_fn)
}

# L. provincialis
search_L.provincialis_fn <- "L.provincialis_FL.Rdata"
if (file.exists(search_L.provincialis_fn)) {
  load(search_L.provincialis_fn)
} else {  
  inat_L.provincialis_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                         taxon_name = "Liatris provincialis",
                                         year = NULL,
                                         month = NULL,
                                         quality_grade = "research",
                                         maxresults = 1000)
  save(inat_L.provincialis_df, file = search_L.provincialis_fn)
}

# L. quadriflora
search_L.quadriflora_fn <- "L.quadriflora_FL.Rdata"
if (file.exists(search_L.quadriflora_fn)) {
  load(search_L.quadriflora_fn)
} else {  
  inat_L.quadriflora_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                        taxon_name = "Liatris quadriflora",
                                        year = NULL,
                                        month = NULL,
                                        quality_grade = "research",
                                        maxresults = 1000)
  save(inat_L.quadriflora_df, file = search_L.quadriflora_fn)
}

# L. resinosa
search_L.resinosa_fn <- "L.resinosa_FL.Rdata"
if (file.exists(search_L.resinosa_fn)) {
  load(search_L.resinosa_fn)
} else {  
  inat_L.resinosa_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                     taxon_name = "Liatris spicata var. resinosa",
                                     year = NULL,
                                     month = NULL,
                                     maxresults = 1000)
  save(inat_L.resinosa_df, file = search_L.resinosa_fn)
}

# L. secunda
search_L.secunda_fn <- "L.secunda_FL.Rdata"
if (file.exists(search_L.secunda_fn)) {
  load(search_L.secunda_fn)
} else {  
  inat_L.secunda_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                    taxon_name = "Liatris secunda",
                                    year = NULL,
                                    month = NULL,
                                    quality_grade = "research",
                                    maxresults = 1000)
  save(inat_L.secunda_df, file = search_L.secunda_fn)
}

# L. squarrulosa
search_L.squarrulosa_fn <- "L.squarrulosa_FL.Rdata"
if (file.exists(search_L.squarrulosa_fn)) {
  load(search_L.squarrulosa_fn)
} else {  
  inat_L.squarrulosa_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                        taxon_name = "Liatris squarrulosa",
                                        year = NULL,
                                        month = NULL,
                                        quality_grade = "research",
                                        maxresults = 1000)
  save(inat_L.squarrulosa_df, file = search_L.squarrulosa_fn)
}

# L. tenuifolia
search_L.tenuifolia_fn <- "L.tenuifolia_FL.Rdata"
if (file.exists(search_L.tenuifolia_fn)) {
  load(search_L.tenuifolia_fn)
} else {  
  inat_L.tenuifolia_df <- get_inat_obs(bounds = FL_bb[c(2,1,4,3)], 
                                       taxon_name = "Liatris tenuifolia",
                                       year = NULL,
                                       month = NULL,
                                       quality_grade = "research",
                                       maxresults = 1000)
  save(inat_L.tenuifolia_df, file = search_L.tenuifolia_fn)
}


################ Saves iNat Data into Data Frame  ##############################
################ Selects Key Columns & Combines with Map Data ##################

# L. aspera
inat_L.aspera_pcsp_popup_sf <- inat_L.aspera_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. chapmanii
inat_L.chapmanii_pcsp_popup_sf <- inat_L.chapmanii_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. elegantula
inat_L.elegantula_pcsp_popup_sf <- inat_L.elegantula_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. garberi
inat_L.garberi_pcsp_popup_sf <- inat_L.garberi_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. gholsonii
inat_L.gholsonii_pcsp_popup_sf <- inat_L.gholsonii_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. laevigata
inat_L.laevigata_pcsp_popup_sf <- inat_L.laevigata_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. ohlingerae
inat_L.ohlingerae_pcsp_popup_sf <- inat_L.ohlingerae_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. patens
inat_L.patens_pcsp_popup_sf <- inat_L.patens_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. pauciflora
inat_L.pauciflora_pcsp_popup_sf <- inat_L.pauciflora_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. provincialis
inat_L.provincialis_pcsp_popup_sf <- inat_L.provincialis_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. quadriflora
inat_L.quadriflora_pcsp_popup_sf <- inat_L.quadriflora_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. resinosa
inat_L.resinosa_pcsp_popup_sf <- inat_L.resinosa_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. secunda
inat_L.secunda_pcsp_popup_sf <- inat_L.secunda_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. squarrulosa
inat_L.squarrulosa_pcsp_popup_sf <- inat_L.squarrulosa_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

# L. tenuifolia
inat_L.tenuifolia_pcsp_popup_sf <- inat_L.tenuifolia_df %>% 
  select(longitude, latitude, datetime, common_name, scientific_name, 
         image_url, user_login) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(FL_sf) %>% 
  mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                             "<i>", scientific_name, "</i></p>",
                             "<p>Observed: ", datetime, "<br/>",
                             "User: ", user_login, "</p>",
                             "<p><img src='", image_url, 
                             "' style='width:100%;'/></p>"))

############################# Creates Map  #####################################

htmltools::p("Native Liatris in Florida", 
             style = "font-weight:bold; font-size:110%;")

Liatris = leaflet(FL_sf, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("GeoportailFrance.orthos") %>% 
  addPolygons(fill = TRUE, fillColor = "forestgreen", color = "black", 
              weight = 1, fillOpacity = 0.4) %>% 
  # Adding Liatris species
  addCircleMarkers(data = inat_L.aspera_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#E41A1C", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.chapmanii_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#377EB8", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.elegantula_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#4DAF4A", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.garberi_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#984EA3", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.gholsonii_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#FF7F00", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.laevigata_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#FFFF33", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.ohlingerae_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#A65628", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.patens_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#F781BF", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.pauciflora_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#999999", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.provincialis_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#66C2A5", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.quadriflora_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#FC8D62", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.secunda_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#E78AC3", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.squarrulosa_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#A6D854", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  addCircleMarkers(data = inat_L.tenuifolia_pcsp_popup_sf,
                   popup = ~popup_html, weight = 1, 
                   radius = 3.5, fill = TRUE, fillColor = "#FFD92F", opacity = 1,
                   fillOpacity = 1, color = "black") %>%
  # Adding legend
  addLegend(opacity = 1, position = 'bottomleft', 
            colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                       "#FF7F00", "#FFFF33", "#A65628", "#F781BF", 
                       "#999999", "#66C2A5", "#FC8D62",  
                       "#E78AC3", "#A6D854", "#FFD92F"),
            labels = c("L. aspera", "L. chapmanii", "L. elegantula", 
                       "L. garberi", "L. gholsonii", "L. laevigata", 
                       "L. ohlingerae", "L. patens", "L. pauciflora", 
                       "L. provincialis", "L. quadriflora",  
                       "L. secunda", "L. squarrulosa", "L. tenuifolia"),
            title = "Research Grade iNat Observations <br> of Native Liatris spp.")
Liatris
saveWidget(widget = Liatris,
           file = "Liatris.html",
           selfcontained = TRUE)

library(sf)
library(dplyr)

# Load Florida county boundaries
FL_counties_sf <- st_read("geojson-fl-counties-fips.json")

# Intersect observations with Florida counties
L.aspera_counties_sf <- FL_counties_sf %>%
  filter(st_intersects(geometry, 
                       inat_L.aspera_df %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
                       sparse = FALSE) %>% apply(1, any))

# Merge the boundaries of counties with observations
L.aspera_range_sf <- st_union(L.aspera_counties_sf)

# Repeat for other species
L.chapmanii_counties_sf <- FL_counties_sf %>%
  filter(st_intersects(geometry, 
                       inat_L.chapmanii_df %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
                       sparse = FALSE) %>% apply(1, any))

L.chapmanii_range_sf <- st_union(L.chapmanii_counties_sf)

# Combine all ranges and assign unique colors
all_ranges_sf <- rbind(
  st_sf(geometry = L.aspera_range_sf, species = "L. aspera"),
  st_sf(geometry = L.chapmanii_range_sf, species = "L. chapmanii")
)

# Assign colors for species
species_colors <- c("L. aspera" = "#E41A1C", "L. chapmanii" = "#377EB8")

# Create Leaflet map
Liatris <- leaflet(FL_sf, options = leafletOptions(zoomControl = FALSE)) %>% 
  # Add base map and Florida boundaries
  addProviderTiles("GeoportailFrance.orthos") %>% 
  addPolygons(data = FL_sf, 
              fill = TRUE, fillColor = "forestgreen", color = "black", 
              weight = 1, fillOpacity = 0.4, group = "Florida Boundary") %>% 
  
  # Add L. aspera polygon range
  addPolygons(data = all_ranges_sf %>% filter(species == "L. aspera"),
              color = "#E41A1C", 
              fillColor = "#E41A1C",
              fillOpacity = 0.4, weight = 2, 
              popup = ~paste0("<b>Species:</b> ", species),
              group = "L. aspera Range") %>% 
  
  # Add L. chapmanii polygon range
  addPolygons(data = all_ranges_sf %>% filter(species == "L. chapmanii"),
              color = "#377EB8", 
              fillColor = "#377EB8",
              fillOpacity = 0.4, weight = 2, 
              popup = ~paste0("<b>Species:</b> ", species),
              group = "L. chapmanii Range") %>% 
  
  # Add L. aspera observation points
  addCircleMarkers(data = inat_L.aspera_df %>% 
                     st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
                   popup = ~paste0("<b>Species:</b> L. aspera"),
                   radius = 3.5, color = "#E41A1C", 
                   fill = TRUE, fillColor = species_colors["L. aspera"], 
                   fillOpacity = 1, group = "L. aspera Observations") %>% 
  
  # Add L. chapmanii observation points
  addCircleMarkers(data = inat_L.chapmanii_df %>% 
                     st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
                   popup = ~paste0("<b>Species:</b> L. chapmanii"),
                   radius = 3.5, color = "#377EB8", 
                   fill = TRUE, fillColor = species_colors["L. chapmanii"], 
                   fillOpacity = 1, group = "L. chapmanii Observations") %>% 
  
  # Add layer control for toggling layers
  addLayersControl(
    overlayGroups = c("L. aspera Range", "L. chapmanii Range", 
                      "L. aspera Observations", "L. chapmanii Observations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  
  # Add a legend for the species
  addLegend(opacity = 1, position = 'bottomleft', 
            colors = c(species_colors["L. aspera"], species_colors["L. chapmanii"]),
            labels = c("L. aspera Range", "L. chapmanii Range"),
            title = "Liatris Species Ranges")

# Print the map
Liatris


