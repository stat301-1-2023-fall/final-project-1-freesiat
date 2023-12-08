#clean up names of Japanese cities so that all accents are removed
jpn_df$NAME_2 <- iconv(jpn_df$NAME_2, 'utf-8', 'ascii', sub = '')

#filter out all cities in the elderly population dataset that exist in 'jpn_df' and set crs
final_gp_data <- jpn_df |>
  left_join(gp_new, by = join_by(NAME_2 == city_name))

# skimr::skim_without_charts(final_gp_data)

#prepare final_gp_data for analysis
gp_data_sf <- final_gp_data |> 
  filter(!is.na(year_2020_percentage))

#convert healthsites into sf object
jpn_hlthsites_sf <- japan_healthsites |> 
  filter(!is.na(X | Y)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    agr = "constant",
    crs = st_crs(gp_data_sf), 
    stringsAsFactors = FALSE,
    remove = TRUE
  )

#which city does each healthcare facility belong to
joint_data <- st_join(jpn_hlthsites_sf, final_gp_data, join = st_within) |> 
  distinct(geometry, .keep_all = TRUE) |> 
  filter(!is.na(year_2020_percentage))

healthcare_facilities_per_city <- count(as_tibble(joint_data), NAME_2) 

distinct_healthcare_facilities_per_city <- count(as_tibble(joint_data), NAME_2, amenity)

new_data <- left_join(final_gp_data, healthcare_facilities_per_city) |> 
  mutate(density = as.numeric(n / year_2020_percentage))
