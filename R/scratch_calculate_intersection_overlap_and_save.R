install.packages("/Users/tylermuffly/Dropbox (Personal)/tyler_1.1.0.tar.gz", repos = NULL, type = "source")
library(tyler)

library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(tigris)
library(ggplot2)
library(hereR)
library(censusapi)
library(htmlwidgets)
library(webshot)
library(memoise)
library(tidyverse)
library(viridis)
library(wesanderson) # color palettes
library(mapview)
library(htmlwidgets)
library(shiny) # creation of GUI, needed to change leaflet layers with dropdowns
library(htmltools) #added for saving html widget
devtools::install_github('ramnathv/htmlwidgets')
library(htmlwidgets)
library (leaflet.extras)
library (leaflet.minicharts)
library(formattable)
library(tidycensus)

# Set tigris options for using cache
options(tigris_use_cache = TRUE)

# Initialize HERE API
hereR::set_freemium(ans = FALSE)
hereR::set_key("Your-API-Key-Here")
hereR::set_verbose(TRUE)
Sys.setenv(HERE_API_KEY = "VnDX-Rafqchcmb4LUDgEpYlvk8S1-LCYkkrtb1ujOrM")

tidycensus::census_api_key("485c6da8987af0b9829c25f899f2393b4bb1a4fb", overwrite = TRUE)

# Add key to .Renviron. # Reload .Renviron
Sys.setenv(CENSUS_KEY="485c6da8987af0b9829c25f899f2393b4bb1a4fb")
readRenviron("~/.Renviron")





calculate_intersection_overlap_and_save <- function(block_groups, isochrones_joined, drive_time, output_dir) {
  # Filter isochrones for the specified drive time
  isochrones_filtered <- isochrones_joined %>%
    filter(drive_time == drive_time)

  # Calculate intersection
  intersect <- st_intersection(block_groups, isochrones_filtered) %>%
    mutate(intersect_area = st_area(.)) %>%
    select(GEOID, intersect_area) %>%
    st_drop_geometry()

  # Log the progress
  cat("Calculating intersection for", drive_time, "minutes...\n")

  tryCatch(
    {
      # Write the intersection shapefile
      output_shapefile <- file.path(output_dir, paste0("intersect_", drive_time, "_minutes.shp"))
      st_write(intersect, output_shapefile, append = FALSE)
      cat("Intersection calculated and saved successfully.\n")

      # Merge intersection area by GEOID
      block_groups <- left_join(block_groups, intersect, by = "GEOID")

      # Calculate area in all block groups
      block_groups <- block_groups %>%
        mutate(bg_area = st_area(block_groups))

      # Calculate overlap percent between block groups and isochrones
      block_groups <- block_groups %>%
        mutate(
          intersect_area = ifelse(is.na(intersect_area), 0, intersect_area),
          overlap = as.numeric(intersect_area / bg_area)
        )

      # Filter out missing overlap values for quantile calculation
      non_missing_overlap <- block_groups$overlap

      # Summary of the overlap percentiles
      summary_bg <- summary(non_missing_overlap)

      # Print the summary
      cat("Summary of Overlap Percentages for", drive_time, "minutes:\n")
      cat("Min: ", sprintf("%.2f%%\n", summary_bg["Min"]))
      cat("1st Qu.: ", sprintf("%.2f%%\n", summary_bg["1st Qu."]))
      cat("Median: ", sprintf("%.2f%%\n", summary_bg["Median"]))
      cat("Mean: ", sprintf("%.2f%%\n", summary_bg["Mean"]))
      cat("3rd Qu.: ", sprintf("%.2f%%\n", summary_bg["3rd Qu."]))
      cat("Max: ", sprintf("%.2f%%\n", summary_bg["Max"]))

    },
    error = function(e) {
      cat("Error: ", e, "\n")
    }
  )
}

# Use Case


# # Specify the output directory
# output_dir <- "data/shp/"
# block_groups <- "data/shp/block-groups"
# isochrones_joined <- "data/isochrones/isochrones_all_combined/isochrones.shp"
#
# # List of unique drive times for which you want to calculate intersection
#
# drive_time <- st_read("data/isochrones/isochrones_all_combined/isochrones.shp")
# unique_drive_times <- unique(drive_time$range)
# class(unique_drive_times)
#
# # Loop through unique drive times and calculate intersection for each
# for (drive_time in unique_drive_times) {
#   calculate_intersection_overlap_and_save(block_groups,
#                                           isochrones_joined,
#                                           unique_drive_times,
#                                           output_dir)
# }


###################################################################
# Read block groups shapefile
block_groups <- st_read("data/shp/block-groups/US_blck_grp_2021.shp") %>%
  sf::st_set_crs(4326)
# Reproject block_groups to EPSG 4326 - using st_set_crs(4326) does not actually reproject the data from its original CRS to the new CRS (EPSG 4326). Instead, it only changes the metadata of the spatial object to indicate that it is in EPSG 4326 without actually transforming the coordinates.
block_groups <- st_transform(block_groups, crs = 4326)

# Read isochrones shapefile
isochrones_joined <- st_read("data/isochrones/isochrones_all_combined/isochrones.shp") %>%
  sf::st_set_crs(4326)
# Reproject block_groups to EPSG 4326
isochrones_joined <- st_transform(isochrones_joined, crs = 4326)

sf::st_crs(block_groups) == sf::st_crs(isochrones_joined)

###################################################################
# Check for invalid geometries in block_groups
invalid_geometries_bg <- block_groups[!st_is_valid(block_groups), ]

# Check for invalid geometries in isochrones_joined
invalid_geometries_iso <- isochrones_joined[!st_is_valid(isochrones_joined), ]

###################################################################
# Repair invalid geometries in block_groups
block_groups <- st_make_valid(block_groups)

# Repair invalid geometries in isochrones_joined
isochrones_joined <- st_make_valid(isochrones_joined)
plot(isochrones_joined[1]) #sanity check

######################################################
# Add characteristics to isochrones file, save out
# Want to join a plain data frame to the isochrones, not an sf object
isochrones_data <- isochrones_joined %>%
  dplyr::select(drive_time, everything()) %>%
  dplyr::arrange(desc(rank)) #This is IMPORTANT for the layering.

# Map the isochrones to make sure they make sense. We should see one isochrone per point.

#isochrones <- isochrones_data %>% filter(NPI_goba.x==1437595840) #for testing
isochrones$drive_time <- as.factor(isochrones$drive_time)


color_palette <- viridis::magma(length(unique(isochrones$drive_time)))

isochrone_map <- tyler::create_base_map("") %>%
  addProviderTiles("CartoDB.Positron", group = "Greyscale") %>%
  addPolygons(
    data = isochrones,
    fillColor = ~color_palette[match(drive_time, unique(isochrones$drive_time))],
    # Use of color palette
    fillOpacity = 1,
    weight = 0.5,
    smoothFactor = 0.5,
    stroke = TRUE,
    color = "black",
    opacity = 0.2,
    popup = ~paste0("<strong>Isochrone:</strong> ", drive_time, " minutes <br />"),
    group = "Isochrones"
  ) %>%
  leaflet::addCircleMarkers(
    data = gyn_onc,
    radius = 2,
    fill = T,
    fillOpacity = 0.1,
    color = "#1f77b4",
    popup = ~paste0("<strong>Name:</strong> ", name, "<br />",
                    "<strong>Location:</strong> ", city_goba, ", ", state_goba),
    group = "Gynecologic Oncologists"
  ) %>%
  addLegend(
    data = isochrones,
    position = "bottomright",
    colors = color_palette,
    labels = unique(isochrones$drive_time),  # Corrected to use drive_time
    title = "Drive Time (minutes)",
    opacity = 0.3
  ) %>%
  addLayersControl(options = layersControlOptions(collapsed = FALSE),
                   overlayGroups = c("Isochrones", "Gynecologic Oncologists"))

isochrone_map

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

html_file <- paste0("figures/isochrone_map_", timestamp, ".html")
png_file <- paste0("figures/isochrone_map_", timestamp, ".png")
htmlwidgets::saveWidget(widget = isochrone_map, file = html_file,
                        selfcontained = TRUE)
cat("Leaflet map saved as HTML:", html_file, "\n")
#webshot(html_file, file = png_file)
#cat("Screenshot saved as PNG:", png_file, "\n")


###################################################################
# USE CASE
# Specify the output directory
output_dir <- "data/shp/"

# List of unique drive times for which you want to calculate intersection
unique_drive_times <- unique(isochrones_joined$range)

# Loop through unique drive times and calculate intersection for each
for (drive_time in unique_drive_times) {
  calculate_intersection_overlap_and_save(block_groups,
                                          isochrones_joined,
                                          drive_time,  # Use drive_time as a single value
                                          output_dir)
}



### testing area

library(tyler)
library(sf)
library(viridis)
library(leaflet)

library(tyler)
library(sf)
library(viridis)
library(leaflet)

library(tyler)
library(sf)
library(viridis)
library(leaflet)

create_and_save_physician_dot_map <- function(physician_data, jitter_range = 0.05, color_palette = "magma", popup_var = "name") {

  # jittered_physician_data = geocoded_data_with_ACOG_districts %>% filter(!is.na(ACOG_District))
  # class(physician_data)
  # color_palette = "magma"
  # jitter_range = 0.05
  # popup_var = geocoded_data_with_ACOG_districts$address

  # if (inherits(physician_data, "sf")) {
  #   # If physician_data is already an sf object, use it directly
  #   jittered_physician_data <- physician_data
  # } else if (is.data.frame(physician_data) && all(c("lat", "long") %in% colnames(physician_data))) {
  #   # If physician_data is a data frame with lat and long columns, convert it to an sf object
  #   jittered_physician_data <- st_as_sf(physician_data, coords = c("long", "lat"), crs = 4326)
  # } else {
  #   stop("Input physician_data must be an sf object or a data frame with 'lat' and 'long' columns.")
  # }

  # Add jitter to latitude and longitude coordinates
  jittered_physician_data <- jittered_physician_data %>%
    dplyr::mutate(
      lat = lat + runif(n()) * jitter_range,
      long = long + runif(n()) * jitter_range
    )

  # Create a base map using tyler::create_base_map()
  cat("Setting up the base map...\n")
  base_map <- tyler::create_base_map("Physician Dot Map")
  cat("Map setup complete.\n")

  # Generate ACOG districts using tyler::generate_acog_districts_sf()
  cat("Generating the ACOG district boundaries from tyler::generate_acog_districts_sf...\n")
  acog_districts <- tyler::generate_acog_districts_sf()

  # Define the number of ACOG districts
  num_acog_districts <- 11

  # Create a custom color palette using viridis
  district_colors <- viridis::viridis(num_acog_districts, option = color_palette)

  # Reorder factor levels
  jittered_physician_data$ACOG_District <- factor(
    jittered_physician_data$ACOG_District,
    levels = c("District I", "District II", "District III", "District IV", "District V",
               "District VI", "District VII", "District VIII", "District IX",
               "District XI", "District XII"))

  # Create a Leaflet map
  dot_map <- base_map %>%
    # Add physician markers
    leaflet::addCircleMarkers(
      data = jittered_physician_data,
      lng = ~long,
      lat = ~lat,
      radius = 3,         # Adjust the radius as needed
      stroke = TRUE,      # Add a stroke (outline)
      weight = 1,         # Adjust the outline weight as needed
      #color = district_colors[as.numeric(physician_data$ACOG_District)],   # Set the outline color to black
      fillOpacity = 0.8#,  # Fill opacity
      #popup = as.formula(paste0("~", popup_var))  # Popup text based on popup_var argument
    ) %>%
    # Add ACOG district boundaries
    leaflet::addPolygons(
      data = acog_districts,
      color = "red",      # Boundary color
      weight = 2,         # Boundary weight
      fill = FALSE,       # No fill
      opacity = 0.8#,      # Boundary opacity
      #popup = ~ACOG_District   # Popup text
    ) #%>%
    # Add a legend
    # leaflet::addLegend(
    #   position = "bottomright",   # Position of the legend on the map
    #   colors = district_colors,   # Colors for the legend
    #   labels = levels(physician_data$ACOG_District),   # Labels for legend items
    #   title = "ACOG Districts"   # Title for the legend
    # )

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Define file names with timestamps
  html_file <- paste0("figures/dot_map_", timestamp, ".html")
  png_file <- paste0("figures/dot_map_", timestamp, ".png")

  # Save the Leaflet map as an HTML file
  htmlwidgets::saveWidget(widget = dot_map, file = html_file, selfcontained = TRUE)
  cat("Leaflet map saved as HTML:", html_file, "\n")

  # Capture and save a screenshot as PNG
  webshot::webshot(html_file, file = png_file)
  cat("Screenshot saved as PNG:", png_file, "\n")

  # Return the Leaflet map
  return(dot_map)
}



geocoded_data <- readr::read_csv("/Users/tylermuffly/Dropbox (Personal)/workforce/Master_References/NPPES/NPPES_November_filtered_data_for_geocoding_geocoded_addresses.csv") %>%
  mutate(id = 1:n()) %>%
  mutate(postal_code = stringr::str_sub(postal_code,1 ,5)) %>%
  mutate(access = exploratory::str_remove(access, regex("^POINT \\(", ignore_case = TRUE), remove_extra_space = TRUE)) %>%
  mutate(access = exploratory::str_remove(access, regex("\\)$", ignore_case = TRUE), remove_extra_space = TRUE)) %>%
  separate(access, into = c("lat", "long"), sep = "\\s+", convert = TRUE) %>%
  select(-id, -rank, -type, -district, -state) %>%
  filter(country %in% c("United States", "Puerto Rico")) %>%
  mutate(postal_code = str_sub(postal_code,1 ,5)) %>%
  rename(zip = postal_code) %>%
  mutate(across(c(lat, long), parse_number)) %>%
  filter(!is.na(lat)) %>%
  filter(-col, -expected, -actual)

ACOG_Districts <- tyler::ACOG_Districts

geocoded_data_with_ACOG_districts <- geocoded_data %>%
  left_join(`ACOG_Districts`, by = join_by(`state_code` == `State_Abbreviations`))

create_and_save_physician_dot_map(physician_data = geocoded_data_with_ACOG_districts,
                                         color_palette = "magma",
                                        jitter_range = 0.05,
                                         popup_var = NULL)
names(geocoded_data_with_ACOG_districts)
class(geocoded_data_with_ACOG_districts)


###################################################################
# Not working: base_map,


# Load the required libraries
library(leaflet)

# Create a data frame with latitude and longitude coordinates
locations <- geocoded_data_with_ACOG_districts

# Create a Leaflet map
leaflet(geocoded_data_with_ACOG_districts %>% head(100)) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lat,
    lat = ~long,
    radius = 3,         # Adjust the radius as needed
    stroke = TRUE,      # Add a stroke (outline)
    weight = 1,         # Adjust the outline weight as needed
    fillOpacity = 0.8,  # Fill opacity
    color = district_colors[as.numeric(geocoded_data_with_ACOG_districts$ACOG_District)],   # Set the outline color to black

  )



# Create a Leaflet map
dot_map <- base_map %>%
  # Add physician markers
  leaflet::addCircleMarkers(
    data = jittered_physician_data,
    lng = ~long,
    lat = ~lat,
    radius = 3,         # Adjust the radius as needed
    stroke = TRUE,      # Add a stroke (outline)
    weight = 1,         # Adjust the outline weight as needed
    #color = district_colors[as.numeric(physician_data$ACOG_District)],   # Set the outline color to black
    fillOpacity = 0.8#,  # Fill opacity
    #popup = as.formula(paste0("~", popup_var))  # Popup text based on popup_var argument
  ) %>%
  # Add ACOG district boundaries
  leaflet::addPolygons(
    data = acog_districts,
    color = "red",      # Boundary color
    weight = 2,         # Boundary weight
    fill = FALSE,       # No fill
    opacity = 0.8#,      # Boundary opacity
    #popup = ~ACOG_District   # Popup text
  ) #%>%
# Add a legend
# leaflet::addLegend(
#   position = "bottomright",   # Position of the legend on the map
#   colors = district_colors,   # Colors for the legend
#   labels = levels(physician_data$ACOG_District),   # Labels for legend items
#   title = "ACOG Districts"   # Title for the legend
# )
