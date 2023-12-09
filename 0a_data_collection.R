## Data quality & complexity check

#load packages
library(tidyverse)
library(skimr)
library(readxl)
library(raster)

#all healthcare sites in Japan
japan_healthsites <- read_csv("data/raw_data/japan.csv") |> 
  dplyr::select(X, Y, amenity)

#percentage of citizens over 65 years old in every city in Japan
geriatric_population <- read_excel("data/raw_data/geriatric_city.xlsx")
janitor::clean_names(geriatric_population)  

#dataframe of japan with all cities
jpn <- getData("GADM", country = "JPN", level = 2)
jpn_df <- sf::st_as_sf(jpn, region = "NAME_2")
janitor::clean_names(jpn_df) 

#number of citizens over 65 years old in each of Japan's prefectures
prefecture_gp <- read_xlsx("data/raw_data/prefecture_population.xlsx") 
janitor::clean_names(prefecture_gp)

#overview of variable types and missingness
skimr::skim_without_charts(japan_healthsites)
skimr::skim_without_charts(geriatric_population)
skimr::skim_without_charts(jpn_df)
