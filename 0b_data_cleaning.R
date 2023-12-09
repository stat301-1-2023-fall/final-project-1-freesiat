# Data Cleaning
library(tidyverse)
library(skimr)
library(readxl)
library(raster)

#Clean up city names
#drop "city" from city name 
gp_new <- as.data.frame(geriatric_population) |> 
  mutate(city_name = word(city, 1)) |> 
  dplyr::select(year_2020_percentage, city_name) 

#clean up names of Japanese cities so that all accents are removed
jpn_df$NAME_2 <- iconv(jpn_df$NAME_2, 'utf-8', 'ascii', sub = '')

#filter out all cities in the elderly population dataset that exist in 'jpn_df'
final_gp_data <- jpn_df |>
  left_join(gp_new, by = join_by(NAME_2 == city_name))

#prepare final_gp_data for analysis by removing all rows that had missing values
gp_data_sf <- final_gp_data |> 
  filter(!is.na(year_2020_percentage))

#cleaning up prefecture-based geriatric population data
colnames(prefecture_gp)[1] <- "prefecture"

#finding total geriatric population by adding together the columns, and then multiplying by 1000 since the data is in 1000's of people 
prefecture_gp_sum <- prefecture_gp |> 
  mutate(sum = rowSums(across(where(is.numeric)), na.rm=TRUE)*1000,
         prefecture_new = gsub("-.*","", prefecture_gp$prefecture)) |> 
  dplyr::select(prefecture_new, sum)


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

#df showing the number of healthcare facilities per city
healthcare_facilities_per_city <- count(as_tibble(joint_data), NAME_2) 
write_csv(healthcare_facilities_per_city, 
          "data/healthcare_facilities_per_city.csv")

#df showing the number of distinct healthcare facilities per city
distinct_healthcare_facilities_per_city <- count(as_tibble(joint_data), NAME_2, amenity)
write_csv(distinct_healthcare_facilities_per_city, 
          "data/distinct_healthcare_facilities_per_city.csv")

#combine geriatric population data, healthcare facility data, and prefecture data
geriatric_pop_healthcare_facility_data <- left_join(final_gp_data, distinct_healthcare_facilities_per_city) |> 
  left_join(prefecture_gp_sum, join_by(NAME_1 == prefecture_new)) |> 
  mutate(density = as.numeric(n / year_2020_percentage)) 

#export combined df as csv file
write_csv(geriatric_pop_healthcare_facility_data, 
          "data/geriatric_pop_healthcare_facility_data.csv")

