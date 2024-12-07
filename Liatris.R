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

species_list <- c("L.aspera", "L.chapmanii", "L.elegantula", "L.garberi", 
                  "L.gholsonii", "L.laevigata", "L.ohlingerae", "L.patens", 
                  "L.pauciflora", "L.provincialis", "L.quadriflora", 
                  "L.resinosa", "L.secunda", "L.squarrulosa", "L.tenuifolia")


for (species in species_list) {
  # Construct dynamic file name
  search_fn <- paste0(species, "_FL.Rdata")
  
  # Check if file exists
  if (file.exists(search_fn)) {
    load(search_fn)
  } else {
    # Construct dynamic taxon name
    taxon_name <- paste0("Liatris", species)
    
    # Get iNaturalist observations
    inat_df_name <- paste0("inat_", species, "_df")
    assign(inat_df_name, get_inat_obs(bounds = FL_bb[c(2,1,4,3)],
                                      taxon_name = taxon_name,
                                      year = NULL,
                                      month = NULL,
                                      quality_grade = "research",
                                      maxresults = 1000))
    
    # Save the data
    save(list = inat_df_name, file = search_fn)
  }
}


for (species in species_list) {
  # Construct dynamic data frame names
  inat_df_name <- paste0("inat_", species, "_df")
  pcsp_popup_sf_name <- paste0("inat_", species, "_pcsp_popup_sf")
  
  # Apply the operations
  assign(pcsp_popup_sf_name, 
         get(inat_df_name) %>%
           select(longitude, latitude, datetime, common_name, scientific_name, image_url, user_login) %>%
           st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
           st_intersection(FL_sf) %>%
           mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
                                      "<i>", scientific_name, "</i></p>",
                                      "<p>Observed: ", datetime, "<br/>",
                                      "User: ", user_login, "</p>",
                                      "<p><img src='", image_url,
                                      "' style='width:100%;'/></p>"))
  )
}


############################# Creates Map  #####################################
# Assign colors for species
species_colors <- c(
  "L. aspera" = "#E41A1C",
  "L. chapmanii" = "#377EB8",
  "L. elegantula" = "#4DAF4A",
  "L. garberi" = "#984EA3",
  "L. gholsonii" = "#FF7F00",
  "L. laevigata" = "#FFFF33",
  "L. ohlingerae" = "#A65628",
  "L. patens" = "#F781BF",
  "L. pauciflora" = "#999999",
  "L. provincialis" = "#B3DE69",
  "L. quadriflora" = "#CCEBC5",
  "L. resinosa" = "#DBDBDB",
  "L. secunda" = "#9EDAE5",
  "L. squarrulosa" = "#666666",
  "L. tenuifolia" = "#1B9E77"
)

htmltools::p("Native Liatris in Florida", 
             style = "font-weight:bold; font-size:110%;")

# Initialize a Leaflet map
Liatris_map <- leaflet() %>%
  addTiles()  # Add a basemap layer

# Loop through the species list
for (species in species_list) {
  # Construct dynamic variable names
  inat_sf_name <- paste0("inat_", species, "_pcsp_popup_sf")

  # Extract the dynamic objects
  inat_sf <- get(inat_sf_name)

  # Add species observation points to the map
  Liatris_map <- Liatris_map %>%
    addCircleMarkers(
      data = inat_sf,
      radius = 4,
      color = species_colors[species],
      fillColor = species_colors[species],
      fillOpacity = 1,
      popup = ~popup_html,  # Use pre-created popup HTML
      group = paste0(species, " Observations")
    )
}

Liatris_map
saveWidget(widget = Liatris,
           file = "Liatris.html",
           selfcontained = TRUE)

library(sf)
library(dplyr)

# Load Florida county boundaries
FL_counties_sf <- st_read("geojson-fl-counties-fips.json")

# Define a vector of species names
species_list <- c("L.aspera", "L.chapmanii", "L.elegantula", "L.garberi", 
                  "L.gholsonii", "L.laevigata", "L.ohlingerae", "L.patens", 
                  "L.pauciflora", "L.provincialis", "L.quadriflora", 
                  "L.resinosa", "L.secunda", "L.squarrulosa", "L.tenuifolia")

# Loop through the species list
for (species in species_list) {
  # Construct the data frame name dynamically
  species_df_name <- paste0("inat_", species, "_df")
  
  # Filter counties intersecting with the species' observations
  species_counties_sf <- FL_counties_sf %>%
    filter(st_intersects(geometry,
                         get(species_df_name) %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
                         sparse = FALSE) %>% apply(1, any))
  
  # Create a variable name for the species' range
  species_range_name <- paste0(species, "_range_sf")
  
  # Assign the union of counties to the variable
  assign(species_range_name, st_union(species_counties_sf))
}

# Combine all ranges and assign unique colors
all_ranges_sf <- rbind(
  st_sf(geometry = L.aspera_range_sf, species = "L. aspera"),
  st_sf(geometry = L.chapmanii_range_sf, species = "L. chapmanii"),
  st_sf(geometry = L.elegantula_range_sf, species = "L. elegantula"),
  st_sf(geometry = L.garberi_range_sf, species = "L. garberi"),
  st_sf(geometry = L.gholsonii_range_sf, species = "L. gholsonii"),
  st_sf(geometry = L.laevigata_range_sf, species = "L. laevigata"),
  st_sf(geometry = L.ohlingerae_range_sf, species = "L. ohlingerae"),
  st_sf(geometry = L.patens_range_sf, species = "L. patens"),
  st_sf(geometry = L.pauciflora_range_sf, species = "L. pauciflora"),
  st_sf(geometry = L.provincialis_range_sf, species = "L. provincialis"),
  st_sf(geometry = L.quadriflora_range_sf, species = "L. quadriflora"),
  st_sf(geometry = L.resinosa_range_sf, species = "L. resinosa"),
  st_sf(geometry = L.secunda_range_sf, species = "L. secunda"),
  st_sf(geometry = L.squarrulosa_range_sf, species = "L. squarrulosa"),
  st_sf(geometry = L.tenuifolia_range_sf, species = "L. tenuifolia")
)

# Assign colors for species
species_colors <- c(
  "L. aspera" = "#E41A1C",
  "L. chapmanii" = "#377EB8",
  "L. elegantula" = "#4DAF4A",
  "L. garberi" = "#984EA3",
  "L. gholsonii" = "#FF7F00",
  "L. laevigata" = "#FFFF33",
  "L. ohlingerae" = "#A65628",
  "L. patens" = "#F781BF",
  "L. pauciflora" = "#999999",
  "L. provincialis" = "#B3DE69",
  "L. quadriflora" = "#CCEBC5",
  "L. resinosa" = "#DBDBDB",
  "L. secunda" = "#9EDAE5",
  "L. squarrulosa" = "#666666",
  "L. tenuifolia" = "#1B9E77"
)

# Create Leaflet map
for (species in species_list) {
  # Construct the data frame name dynamically
  species_df_name <- paste0("inat_", species, "_df")
  
  # Construct the range name dynamically
  species_range_name <- paste0(species, "_range_sf")
  
  # Add species range to the map
  Liatris <- Liatris %>% addPolygons(
    data = get(species_range_name),
    color = species_colors[species],
    fillColor = species_colors[species],
    fillOpacity = 0.4, weight = 2,
    popup = ~paste0("<b>Species:</b> ", species),
    group = paste0(species, " Range")
  )
  
  # Add species observations to the map
  Liatris <- Liatris %>% addCircleMarkers(
    data = get(species_df_name) %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
    popup = ~paste0("<b>Species:</b> ", species),
    radius = 3.5, color = species_colors[species],
    fill = TRUE, fillColor = species_colors[species],
    fillOpacity = 1, group = paste0(species, " Observations")
  )
}

# Create lists for layer control and legend
range_groups <- paste0(species_list, " Range")
observation_groups <- paste0(species_list, " Observations")
legend_labels <- paste0(species_list, " Range")

# Update the map with the dynamic layer control and legend
Liatris <- Liatris %>% 
  addLayersControl(
    overlayGroups = c(range_groups, observation_groups),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(opacity = 1, position = 'bottomleft',
            colors = species_colors[species_list],
            labels = legend_labels,
            title = "Liatris Species Ranges")
# Print the map
Liatris


