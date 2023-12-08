## Data quality & complexity check


#all healthcare sites in Japan
japan_healthsites <- read_csv("data/japan.csv") |> 
  dplyr::select(X, Y, amenity)

#number of citizens over 65 years old in every city in Japan
geriatric_population <- read_excel("data/geriatric_city.xlsx")
janitor::clean_names(geriatric_population)  

#drop "city" from city name 
gp_new <- as.data.frame(geriatric_population) |> 
  mutate(city_name = word(city, 1)) |> 
  dplyr::select(year_2020_percentage, city_name) 

#dataframe of japan with all cities
jpn <- getData("GADM", country = "JPN", level = 2)
jpn_df <- sf::st_as_sf(jpn, region = "NAME_2")
janitor::clean_names(jpn_df) 

#overview of variable types and missingness
skimr::skim_without_charts(japan_healthsites)
skimr::skim_without_charts(gp_new)
skimr::skim_without_charts(jpn_df)
