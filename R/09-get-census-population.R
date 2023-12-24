
# 4.  Get population data for each block group
### Calculate populations within and further than a 45-minute drive

#We'll get the population in each block group using the [censusapi](https://www.hrecht.com/censusapi/) library. If you're interested in stratifying by stroke center type or race, see the [original project code](https://github.com/khnews/2021-delta-appalachia-stroke-access). Here we'll just look at overall population within 45-minutes of any stroke center.

#```{r, eval = FALSE}
 # "B01001_026E  Estimate!!Total!!Female                      \n",
 #       "B01001_027E  Estimate!!Total!!Female!!Under 5 years       ",
 #       "B01001_028E  Estimate!!Total!!Female!!5 to 9 years        \n",
 #       "B01001_029E  Estimate!!Total!!Female!!10 to 14 years      \n",
 #       "B01001_030E  Estimate!!Total!!Female!!15 to 17 years      \n",
 #       "B01001_031E  Estimate!!Total!!Female!!18 and 19 years     \n",
 #       "B01001_032E  Estimate!!Total!!Female!!20 years            \n",
 #       "B01001_033E  Estimate!!Total!!Female!!21 years            \n",
 #       "B01001_034E  Estimate!!Total!!Female!!22 to 24 years      \n",
 #       "B01001_035E  Estimate!!Total!!Female!!25 to 29 years      \n",
 #       "B01001_036E  Estimate!!Total!!Female!!30 to 34 years      \n",
 #       "B01001_037E  Estimate!!Total!!Female!!35 to 39 years      \n",
 #       "B01001_038E  Estimate!!Total!!Female!!40 to 44 years      \n",
 #       "B01001_039E  Estimate!!Total!!Female!!45 to 49 years      \n",
 #       "B01001_040E  Estimate!!Total!!Female!!50 to 54 years      \n",
 #       "B01001_041E  Estimate!!Total!!Female!!55 to 59 years      \n",
 #       "B01001_042E  Estimate!!Total!!Female!!60 and 61 years     \n",
 #       "B01001_043E  Estimate!!Total!!Female!!62 to 64 years      \n",
 #       "B01001_044E  Estimate!!Total!!Female!!65 and 66 years     \n",
 #       "B01001_045E  Estimate!!Total!!Female!!67 to 69 years      \n",
 #       "B01001_046E  Estimate!!Total!!Female!!70 to 74 years      \n",
 #       "B01001_047E  Estimate!!Total!!Female!!75 to 79 years      \n",
 #       "B01001_048E  Estimate!!Total!!Female!!80 to 84 years      \n",
 #       "B01001_049E  Estimate!!Total!!Female!!85 years and over   \n",
# ```
#
#
# ```{r, eval = TRUE}
library(tigris)
us_fips_list <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  dplyr::distinct(state_code, .keep_all = TRUE) %>%
  dplyr::filter(state_code < 56) %>%
  dplyr::select(state_code) %>%
  dplyr::pull()

all_census_data <- tyler::get_census_data(us_fips = us_fips_list)

#The GEOID for block groups in the United States can be constructed using the following format: STATECOUNTYTRACTBLOCK_GROUP. Specifically:
#
# STATE is a 2-digit code for the state.
# COUNTY is a 3-digit code for the county.
# TRACT is a 6-digit code for the census tract.
# BLOCK_GROUP is a 1-digit code for the block group within the tract.

demographics_bg <- all_census_data %>%
  dplyr::rename(name = NAME,
         population = B01001_026E,
         #total female population
         fips_state = state) %>%
  dplyr::mutate(fips_block_group = paste0(
    fips_state,
    county,
    str_pad(tract, width = 6, pad = "0"),
    block_group
  ),) %>%
  dplyr::arrange(fips_state) %>%
  dplyr::select(fips_block_group, name, population)

head(demographics_bg)
# ```
#
# And finally! Multiply the population of each block group by its overlap percent to calculate population within and not within ???45???-minutes of a gynecologic oncologist.
#
# First, make a flat non-sf dataframe with the overlap information and join to population. Then multiply and summarize. This is the easiest part of the project.
#
# ```{r calculate_population, include=TRUE}
bg_overlap <- block_groups %>% dplyr::select(geoid = GEOID, overlap) %>%
	sf::st_drop_geometry()
bg_overlap <- as.data.frame(bg_overlap)
write.csv(bg_overlap, "data/09-get-census-population/block-group-isochrone-overlap.csv", na = "", row.names = F)
write_rds(bg_overlap, "data/09-get-census-population/bg_overlap.rds")

#geoid: comma delimited list. (no quotes, leading zeros are necessary) example: '08' is Colorado, '08031' is Denver County , '08031000701' is a Census Tract in Denver County, and '080010078011' is a block group in Adams County

write_rds(demographics_bg, "data/09-get-census-population/demographics_bg.rds") #Census Block Group Code

class(bg_overlap$geoid) == class(demographics_bg$fips_block_group)

# Join data
bg <- left_join(bg_overlap, demographics_bg, by = c("geoid" = "fips_block_group"))
bg

# Calculate!
state_sums <- bg %>% select(geoid, overlap, population) %>%
	summarize(within_total = sum(population * overlap, na.rm = TRUE),
						population_total = sum(population, na.rm = TRUE)) %>%
	mutate(within_total_pct = within_total/population_total) %>%
	ungroup()

head(state_sums)
within_total_pct <- round(state_sums[[3]]*100, 2)
n_population <- format(round(state_sums[[1]], 0), big.mark = ",")

paste0(within_total_pct, "% (N = ", n_population, ") of US female residents live within ??45?? minutes of a gynecologic oncologist, while the remaining", 100L - within_total_pct, "% live further away.")
