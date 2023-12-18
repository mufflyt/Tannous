#######################
source("R/01-setup.R")
#######################

##########################################################################
# search_by_taxonomy
# This will allow us to get subspecialty names and NPI numbers
go_search_by_taxonomy_data <- search_by_taxonomy("Gynecologic Oncology")
fpmrs_search_by_taxonomy_data <- search_by_taxonomy("Female Pelvic Medicine and Reconstructive Surgery")
rei_search_by_taxonomy_data <- search_by_taxonomy("Reproductive Endocrinology")
mfm_search_by_taxonomy_data <- search_by_taxonomy("Maternal & Fetal Medicine")

# Merge all data frames into one
all_taxonomy_search_data <- dplyr::bind_rows(
  go_search_by_taxonomy_data,
  fpmrs_search_by_taxonomy_data,
  rei_search_by_taxonomy_data,
  mfm_search_by_taxonomy_data) %>%
  dplyr::distinct(npi, .keep_all = TRUE)


cleaned_all_taxonomy_search_data <-
  all_taxonomy_search_data %>%
  distinct(npi, .keep_all = TRUE) %>%
  # Keep only the OBGYN subspecialist taxonomy descriptions.
  filter(taxonomies_desc %in% c("Obstetrics & Gynecology, Female Pelvic Medicine and Reconstructive Surgery", "Obstetrics & Gynecology, Gynecologic Oncology", "Obstetrics & Gynecology, Maternal & Fetal Medicine", "Obstetrics & Gynecology, Reproductive Endocrinology")) %>%
  mutate(addresses_postal_code = str_sub(addresses_postal_code,1 ,5)) %>% # Extract the first five of the zip code
  mutate(basic_enumeration_date = ymd(basic_enumeration_date)) %>%
  mutate(basic_enumeration_date_year = year(basic_enumeration_date), .after = ifelse("basic_enumeration_date" %in% names(.), "basic_enumeration_date", last_col())) %>% # Pull the year out of the enumeration full data.
  mutate(basic_middle_name = str_sub(basic_middle_name,1 ,1)) %>%
  mutate(across(c(basic_first_name, basic_last_name, basic_middle_name), .fns = ~str_remove_all(., "[[\\p{P}][\\p{S}]]"))) %>%
  # Get data ready to add these taxonomy rows to the `search_npi`/GOBA data set.
  rename(NPI = npi, first_name = basic_first_name, last_name = basic_last_name, middle_name = basic_middle_name, GenderPhysicianCompare = basic_gender, sub1 = taxonomies_desc, city = addresses_city, state = addresses_state, name.x = full_name, `Zip CodePhysicianCompare` = addresses_postal_code) %>%
  mutate(GenderPhysicianCompare = recode(GenderPhysicianCompare, "F" = "Female", "M" = "Male")) %>%

  # Show the subspecialty from goba.
  mutate(sub1 = recode(sub1, "Obstetrics & Gynecology, Female Pelvic Medicine and Reconstructive Surgery" = "FPM", "Obstetrics & Gynecology, Gynecologic Oncology" = "ONC", "Obstetrics & Gynecology, Maternal & Fetal Medicine" = "MFM", "Obstetrics & Gynecology, Reproductive Endocrinology" = "REI"))

dim(cleaned_all_taxonomy_search_data)
glimpse(cleaned_all_taxonomy_search_data)

write_rds(cleaned_all_taxonomy_search_data, "data/02-search_taxonomy/end_cleaned_all_taxonomy_search_data.rds")

#**********************************************
# SANITY CHECK
#**********************************************
dim(cleaned_all_taxonomy_search_data)
glimpse(cleaned_all_taxonomy_search_data)
janitor::tabyl(cleaned_all_taxonomy_search_data$sub1)
janitor::tabyl(cleaned_all_taxonomy_search_data$GenderPhysicianCompare)
janitor::tabyl(cleaned_all_taxonomy_search_data$state)
