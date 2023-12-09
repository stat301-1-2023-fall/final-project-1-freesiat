library(tidyverse)

#number of types of healthcare facilities in japan
ggplot(distinct_healthcare_facilities_per_city, aes(x = amenity)) + 
  geom_bar()

#boxplot
ggplot(pop_healthcare_facility_data, aes(x = amenity, y = n)) + 
  geom_boxplot()

#number of healthcare facilities in each prefecture in japan
ggplot(pop_healthcare_facility_data, aes(x = NAME_1)) +
  geom_bar()

#number of healthcare facilities in each city
ggplot(distinct_healthcare_facilities_per_city, aes(x = n)) + 
  geom_histogram(binwidth = 5)
