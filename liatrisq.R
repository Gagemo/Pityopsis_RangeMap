#########################   Installs Packages  ##############################

list.of.packages <- c("rgbif", "leaflet", "dplyr", "sf", "spocc", "remotes", "rangemap")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##########################   Loads Packages   ##############################

library(rgbif)
library(leaflet)
library(dplyr)
library(sf)
library(spocc)
library(remotes)
library(rangemap)

# Ensure rangemap is installed
remotes::install_github("mauricerangel/rangemap")

library(rangemap)

# Define species list
species_list <- c("Liatris aspera", "Liatris elegans", "Liatris garberi", 
                  "Liatris tenuifolia", "Liatris squarrosa", "Liatris spicata")

# Retrieve GBIF taxon keys
get_gbif_key <- function(species) {
  key <- name_backbone(name = species)$usageKey
  if (is.null(key)) {
    warning(paste("No taxon key found for", species))
  }
  return(key)
}

gbif_keys <- setNames(sapply(species_list, get_gbif_key), species_list)

# Fetch occurrence data
fetch_gbif_data <- function(taxonKey) {
  occ_data(taxonKey = taxonKey, hasCoordinate = TRUE, limit = 500)$data
}

gbif_data_list <- lapply(gbif_keys, fetch_gbif_data)

# Combine and clean data
gbif_data <- bind_rows(gbif_data_list, .id = "species") %>%
  select(species, decimalLongitude, decimalLatitude) %>%
  drop_na()

# Check if data retrieval worked
if (nrow(gbif_data) == 0) {
  stop("No occurrence data retrieved. Check taxon keys and GBIF availability.")
}

# Fetch iNaturalist data
inat_data <- occ(query = species_list, from = "inat", limit = 500)
inat_data <- inat_data$inat$data %>%
  select(name, longitude, latitude) %>%
  rename(species = name, decimalLongitude = longitude, decimalLatitude = latitude)

# Combine both datasets
occurrence_data <- bind_rows(gbif_data, inat_data)

# Define species colors
species_colors <- setNames(rainbow(length(species_list)), species_list)

# Create interactive map
leaflet(occurrence_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~decimalLongitude, ~decimalLatitude, 
    color = ~species_colors[species],
    label = ~species,
    radius = 3,
    opacity = 0.8
  ) %>%
  addLegend(
    position = "bottomright",
    colors = species_colors,
    labels = species_list,
    title = "Species"
  )
