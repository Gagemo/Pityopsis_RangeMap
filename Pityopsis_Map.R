cat("\014") # Clears history

list.of.packages <- c("tidyverse", "sp", "rgdal", "rgdal", "rgeos", "raster", "rgbif",
                      "tmap", "rinat", "leaflet", "conflicted", "sf", "ggmap", "rangemap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

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
library(ggmap)
library(rangemap)
library(rgbif)



conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("count", "dplyr", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("arrange", "dplyr", quiet = TRUE)

# Now we can import the park boundary which is saved as a geojson file. We use 
# the sf package to import and transform the boundary to geographic coordinates, 
# and leaflet to plot it:
FL_sf <- st_read("geojson-fl-counties-fips.json", quiet = TRUE) %>% 
  st_transform(4326)
leaflet(FL_sf) %>% 
  addTiles() %>% 
  addPolygons()

# Next, we get the bounding box of the study area, so we can pass it to the 
# iNaturalist API to tell it which area we’re interested in:
FL_bb <- FL_sf %>% 
  st_bbox()

# We’ll use the bounds argument in get_inat_obs() to specify the extent of our 
# search. bounds takes 4 coordinates. 
# The code below first checks if the data have 
# already been downloaded, to prevent downloading data twice.

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

#Maps

htmltools::p("Native Pityopsis in Florida", 
             style = "font-weight:bold; font-size:110%;")

leaflet(FL_sf, options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("Esri.WorldPhysical") %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>% 
  addCircleMarkers(data = inat_P.aspera_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "black")%>% 
  addCircleMarkers(data = inat_P.aequilifolia_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "green")%>% 
  addCircleMarkers(data = inat_P.flexuosa_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "yellow")%>% 
  addCircleMarkers(data = inat_P.latifolia_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "orange")%>% 
  addCircleMarkers(data = inat_P.graminifolia_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "purple")%>% 
  addCircleMarkers(data = inat_P.oligantha_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "blue")%>% 
  addCircleMarkers(data = inat_P.tracyi_pcsp_popup_sf,
                   popup = ~popup_html, 
                   radius = 1, color = "white") %>%
  addLegend("bottomright", 
            colors = c("black",  "green", "yellow", "orange", "purple", "blue", "white"),
            labels = c("P. aspera", "P. aequilifolia", "P. flexuosa", "P. latifolia", 
                       "P. graminifolia", "P. oligantha", "P. tracyi"),
            title = "Native Pityopsis spp.",
            opacity = 1)


ggplot() +             
  geom_sf(data = FL_sf, fill = "#C3D7A4") +
  geom_sf(data = inat_P.aspera_pcsp_popup_sf, color = "blue")+                                
  theme_void() +                               
  theme(plot.background= element_blank(),     
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank()) 












# Download Kalmia latifolia presence data for the USA
KL <- occ_data(scientificName="Pityopsis aspera", hasCoordinate=TRUE, country="US", hasGeospatialIssue=FALSE)

# Get USA state map
NCstate = map_data("state")

# Create the map
NCstate = map_data("state")
ggplot(NCstate, aes(long, lat)) + geom_polygon() + coord_equal()
ggplot() + coord_fixed(1.1) +
geom_polygon(data = NCstate, aes(x=long, y = lat, group = group),
                      fill="white", col="grey", lwd=0.3) +
geom_point(data=KL$data, aes(x = decimalLongitude, y=decimalLatitude,
                                      color="Presence points"), cex=0.5) + labs(color = "Kalmia latifolia") +
geom_polygon(data = NCstate, aes(x=long, y = lat, group = group),
                      fill=rgb(1,1,1,0), col=rgb(0.8,0.8,0.8,0.3), lwd=0.1) +
theme(legend.position=c(0.14,0.15))

ggsave(filename="distribution of Kalmia_latifolia in USA.png", plot=p, width=17, height=8.5, dpi=300, scale=1, unit="cm")



