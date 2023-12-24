#######################
source("R/01-setup.R")
#######################

## 3.  Calculate how much each Census block group in our area of interest overlaps with the isochrones

# To calculate how many people live within and outside of the drive time isochrones, we'll need to identify the percent of each Census block group that lies within the isochrones.
#
# Note: you might see some simplified analyses that look at block group centroids instead of calculating the overlap. If you're working with a massive dataset that type of approach can make sense. But there's no need to simplify so much here.
#
# The block group shapefile is from the 2021 ACS via [NHGIS](https://www.nhgis.org/). This file is clipped to shorelines but not interior bodies of water like lakes. It was clipped to water using the `tigris::erase_water()`.
#
# Block group boundaries do not follow lake or other inland water boundaries. So if a lake takes up 75% of the block group, that makes a big difference for drive time calculations. Exclude water, since people don't live in those lakes and ponds --- unless you're in an area with a lot of people in houseboats.
#
# First, make our block groups and isochrones planar and then valid for computation (fixing any minor shape issues) using `st_make_valid()`.
#
# ```{r prep_shp, include=TRUE}
# Define file paths
block_groups_file <- "data/shp/block-groups/colorado/"

# Read, transform, and process block groups shapefile in one chain
block_groups <- sf::st_read(block_groups_file) %>%
  sf::st_transform(2163) %>%
  sf::st_make_valid() %>%
  sf::st_simplify(preserveTopology = FALSE, dTolerance = 1000)

# Write processed block groups shapefile
sf::st_write(block_groups,
             dsn = "data/08-get-block-group-overlap/simplified_colorado",
             layer = "block_groups",
             driver = "ESRI Shapefile",
             quiet = FALSE, append = FALSE)
# ```
#
# ```{r water, include=FALSE}
# TYLER WE WILL NEED TO CHECK THIS OUT!

# block_groups <- block_groups  %>%
#   tigris::erase_water(area_threshold = 0.75)

# Write processed block groups shapefile
# sf::st_write(block_groups, "data/shp/no_water_simplified_us_lck_grp_2021.shp")
# ```

isochrones <- sf::st_read(dsn = "data/07-isochrone-mapping")

#```{r prep_shp2, include=FALSE}
# Assuming 'isochrones' is already loaded and contains spatial data
# Process 'isochrones' shapefile
isochrones <- isochrones %>%
  sf::st_transform(2163) %>%
  sf::st_make_valid() %>%
  sf::st_simplify(preserveTopology = FALSE, dTolerance = 1000)
#```

#```{r prep_shp2_write, include=FALSE}
# Write processed isochrones
#sf::st_write(isochrones, "data/shp/simplified_isochrones.shp")
#```

# For computation, we don't care about individual isochrones, many of which overlap. Instead we just want to know how a block group fits in with any of the isochrones period, not each individual one. So combine them into one single feature with `st_union()` and then make into a nice sf object with `st_sf()`.
# ```{r join isochrones, include=TRUE}
isochrones_joined <- sf::st_union(isochrones)
isochrones_joined <- sf::st_sf(iso_id = 1, geometry = isochrones_joined) %>%
	sf::st_transform(2163)
#```

### Single Isochrone Coverage Map
#Make another quick reference map to make sure you have just one single isochrones feature. Remember before you could see all the overlapping shapes.

#```{r isochrones_joined_map, include=TRUE}
isochrones_joined_map <- isochrones_joined %>%
  sf::st_transform(4326)
#```

## DOES NOT WORK!!!!
#```{r, eval=FALSE}
# Usage example:
# List of unique drive times for which you want to create plots and shapefiles
drive_times <- unique(isochrones$range)

isochrones <- isochrones %>%
  rename("drive_time" ="range")
tyler::create_individual_isochrone_plots(isochrones, drive_times)
#```


### Filter GYN Oncologists with `st_intersects`
#Important note: for computations in sf we should use a planar projection, [not a lat/long projection](https://r-spatial.github.io/sf/articles/sf6.html#although-coordinates-are-longitudelatitude-xxx-assumes-that-they-are-planar) like we'd use for making Leaflet maps. We'll use projection ESPG [2163](https://epsg.io/2163).Now calculate the percent overlap between each block group and the isochrones.

## Overall, this code is used to calculate and summarize the overlap percentages between block groups and isochrones, which can be useful for spatial analysis and understanding the degree of overlap between these geographic features.

###### EVAL ===== FALSE
#```{r calculate_overlap, include=FALSE}
# Calculate area in all block groups.  It calculates the area of each block group in the block_groups dataset using the sf::st_area function and adds this information as a new column called bg_area in the block_groups data frame.
block_groups <- dplyr::mutate(block_groups, bg_area = sf::st_area(block_groups))

# Calculate intersection - will take some minutes.  It calculates the intersection between the block_groups and isochrones_joined datasets using sf::st_intersection. This operation identifies the areas where the block groups and isochrones overlap spatially.
# It also calculates the area of the intersection and stores it in a new column called intersect_area in the resulting data frame.
# It selects only the GEOID and intersect_area columns and drops the geometry information from the data frame using sf::st_drop_geometry.
intersect <- sf::st_intersection(block_groups, isochrones_joined) %>%
		mutate(intersect_area = st_area(.)) %>%
		select(GEOID, intersect_area) %>%
		sf::st_drop_geometry()

#sf::st_write(intersect, "data/shp/intersect.shp")

# Merge intersection area by geoid.  It performs a left join between the original block_groups data frame and the intersect data frame based on the common column "GEOID." This step adds the intersect_area information to the block_groups data frame.
block_groups <- left_join(block_groups, intersect, by = "GEOID")

# Calculate overlap percent between block groups and isochrones.  It calculates the overlap percentage between each block group and the isochrones by dividing the intersect_area by the bg_area. If there is no intersection (represented by NA in intersect_area), it sets the overlap percentage to 0.
block_groups <- block_groups %>%
	# If missing it's because it didn't overlap, so 0
	mutate(intersect_area = ifelse(is.na(intersect_area), 0, intersect_area),
		overlap = as.numeric(intersect_area/bg_area))

# Summary of the overlap percents.  It calculates a summary of the overlap values in the block_groups data frame using the summary function.
summary(block_groups$overlap)

summary_bg <- summary(block_groups$overlap)

round(summary_bg[[4]], 4) *100

paste0("The isochrones overlap with: ", round(summary_bg[[4]], 4) *100,"% of the block groups in the block groups provided by area. " )

#######
#This function takes the block_groups and isochrones datasets as input, along with the breaks argument for specifying drive times. It calculates overlap percentages for each drive time, saves individual shapefiles, and returns a list of summaries for each drive time.

### NOT WORKING!!!!
#```{r, eval=FALSE}
# Function to calculate intersection between block groups and isochrones,
# calculate overlap percentages, and save to a shapefile
calculate_intersection_overlap_and_save <- function(block_groups, isochrones, drive_time, output_dir) {
  drive_time <- 60L
  output_dir <- "data/"

  # Filter isochrones for the specified drive time
  isochrones_filtered <- isochrones %>%
    filter(drive_time == drive_time)

  # Calculate intersection
  intersect <- st_intersection(block_groups, isochrones_filtered) %>%
    #mutate(intersect_area = st_area(.)) %>%
    select(GEOID, intersect_area) %>%
    st_drop_geometry()

  # Log the progress
  message(paste("Calculating intersection for", drive_time, "minutes..."))

  tryCatch(
    {
      # Write the intersection shapefile
      output_shapefile <- file.path(output_dir, paste0("intersect_", drive_time, "_minutes.shp"))
      st_write(intersect, output_shapefile, append = FALSE)
      message("Intersection calculated and saved successfully.")

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
      message("Summary of Overlap Percentages for", drive_time, "minutes:")
      cat(summary_bg)

      # Calculate and print the 50th percentile of overlap percentages
      #median <- round(quantile(non_missing_overlap, probs = 0.5), 4) * 100
      #message("50th Percentile of Overlap Percentages:", median, "%")

      # Calculate and print the 75th percentile of overlap percentages
      # mean <- round(mean(non_missing_overlap), 4) * 100
      # message("75th Percentile of Overlap Percentages:", mean, "%")

    },
    error = function(e) {
      message("Error: ", e)
    }
  )
}

# List of unique drive times for which you want to calculate intersection
unique_drive_times <- unique(isochrones$drive_time)

# Specify the output directory
output_dir <- "data/shp/"

# Loop through unique drive times and calculate intersection for each
for (drive_time in unique_drive_times) {
  calculate_intersection_overlap_and_save(block_groups, isochrones, drive_time, output_dir)
}

