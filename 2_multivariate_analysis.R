library(tidyverse)
library(cor)
library(ggpubr)
library()

#number of healthcare facilities vs number of elderly people in each prefecture in japan
ggplot(pop_healthcare_facility_data, aes(x = sum, y = n)) +
  geom_point(aes(color = amenity)) 

#percentage of citizens over 65 vs number of healthcare facilities
ggplot(pop_healthcare_facility_data, aes(x = year_2020_percentage, y = n)) +
  geom_point(aes(color = amenity)) 

##all of Japan
density_calc_data <- pop_healthcare_facility_data |> 
  mutate(density = as.numeric(n / year_2020_percentage)) 

all_jpn <- ggplot() +
  geom_sf(data = density_calc_data, aes(group = NAME_2, fill = density)) +
  theme_void() 

#Kanto Region
kanto <- ggplot() +
  geom_sf(data = new_data, aes(group = NAME_2, fill = density)) +
  theme_void() +
  coord_sf(xlim = c(137, 142),
           ylim = c(32, 37))

#by prefecture
by_prefecture <- ggpolot() + 
  geom_sf(data = density_calc_data, aes(group = NAME_1, fill = sum)) +
  theme_void()



  

