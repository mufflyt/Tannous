########################################################
#######################################################
# `created_isochrones` is what is created after the geocoded data is sent to the HERE API.

#######################
source("R/01-setup.R")
#######################

# Validate the file of geocoded data.

##### To do the actual gathering of the isochrones: `process_and_save_isochrones`.  We do this in chunks of 25 because we were losing the entire searched isochrones when one error hit.  There is a 1:1 relationship between isochrones and rows in the `input_file` so to match exactly on row we need no errors.  Lastly, we save as a shapefile so that we can keep the MULTIPOLYGON geometry setting for the sf object making it easier to work with the spatial data in the future for plotting, etc.  I struggled because outputing the data as a dataframe was not easy to write it back to a MULTIPOLYGON.

input_file <- readr::read_csv("data/05-geocode-cleaning/end_inner_join_postmastr_clinician_data.csv") %>%
  dplyr::mutate(id = row_number()) %>%
  dplyr::filter(postmastr.name.x != "Hye In Park, MD")

errors <- test_and_process_isochrones(input_file)
errors

#Output from the `test_and_process_isochrones` function
# Filter out the rows that are going to error out after using the test_and_process_isochrones function.
error_rows <- c(265, 431, 816, 922, 1605, 2049, 2212, 2284, 2308, 2409, 2482, 2735, 2875, 2880, 3150, 3552, 3718)

# filter out rows with errors.
input_file_no_error_rows <- input_file %>%
  dplyr::filter(!id %in% error_rows)

# Call the `process_and_save_isochrones` function with your input_file
isochrones_sf <- process_and_save_isochrones(input_file_no_error_rows)

# Check the dimensions of the final isochrones_data
dim(isochrones_sf)
class(isochrones_sf)

isochrones_df <- sf::st_read("data/isochrones/isochrones_all_combined")

# Clip the isochrones to the USA border.
usa_borders <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  sf::st_set_crs(4326)

isochrones <- isochrones_df %>%
  sf::st_set_crs(4326)

isochrones_sf_clipped <- sf::st_intersection(isochrones, usa_borders)

sf::st_write(
  isochrones_sf_clipped,
  dsn = "data/isochrones/isochrones_all_combined",
  layer = "isochrones",
  driver = "ESRI Shapefile",
  quiet = FALSE, append = FALSE)

sf::st_write(
  isochrones_sf_clipped,
  dsn = "data/06-isochrones/isochrones_all_combined",
  layer = "isochrones",
  driver = "ESRI Shapefile",
  quiet = FALSE, append = FALSE)

######################################
# SANITY CHECK
######################################

#BASIC MAP
end_isochrones_sf_clipped <- sf::st_read("data/06-isochrones/isochrones_all_combined")

# Create a basic Leaflet map
map <- leaflet() %>%
  addTiles() # Add the default map tiles

# Define a cooler color palette (e.g., "viridis")
cool_palette <- viridis::magma(length(end_isochrones_sf_clipped$range))

# Add the isochrones as polygons to the map
map <- map %>%
  addPolygons(
    data = end_isochrones_sf_clipped,
    fillColor = ~cool_palette,
    fillOpacity = 0.5,
    weight = 1,
    color = "black",
    popup = ~paste("ID: ", id, "<br>Range: ", range, " seconds")
  )

# Display the map
map
