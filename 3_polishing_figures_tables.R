library(tidyverse)
library(vtable)
library(ggplot2)
library(ggthemes)

#Data Quality Table
colnames(pop_healthcare_facility_data)[14] <- "2020_geriatric_percentage"
colnames(pop_healthcare_facility_data)[15] <- "facility_type"
colnames(pop_healthcare_facility_data)[16] <- "facility_count"
colnames(pop_healthcare_facility_data)[17] <- "geriatric_count_prefecture"

missing_data_summary <- sumtable(pop_healthcare_facility_data,
         vars = c('2020_geriatric_percentage','facility_type', 'facility_count', 'geriatric_count_prefecture'),
         summ=c('notNA(x)',
                'mean(x)',
                'median(x)',
                'propNA(x)'))

#number of types of healthcare facilities in japan
no_per_amenity <- ggplot(na.omit(distinct_healthcare_facilities_per_city), aes(x = amenity)) + 
  geom_bar(aes(fill = amenity)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
         plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
         legend.title = element_text(face = "bold", family = "georgia", size = 8),
         legend.text = element_text(size = 6),
         axis.title.x = element_text(face = "bold", family = "georgia", size = 9),
         axis.title.y = element_text(face = "bold", family = "georgia", size = 9)) +
  labs(title = "Number of Types of Healthcare Facilities in Japan",
       subtitle = "Hospitals and pharmacies are the most common healthcare facilities",
       x = "Healthcare Facility Type",
       y = "Count",
       fill = "Facility Type")

ggsave("figures/no_per_amenity.png", no_per_amenity)

#boxplot
amenity_pop_healthcare_facility_data <- pop_healthcare_facility_data |> 
  filter(!is.na(facility_type))

amenity_density <- ggplot(amenity_pop_healthcare_facility_data, aes(x = facility_type, y = facility_count)) + 
  geom_boxplot() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
        plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
        legend.title = element_text(face = "bold", family = "georgia", size = 8),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(face = "bold", family = "georgia", size = 9),
        axis.title.y = element_text(face = "bold", family = "georgia", size = 9)) +
  labs(title = "Distribution of Types of Healthcare Facilities in Japan per City",
       subtitle = "Most cities average around 10 facilities",
       x = "Healthcare Facility Type",
       y = "Count")

ggsave("figures/amenity_density.png", amenity_density)

#number of healthcare facilities in each prefecture in japan
no_healthcare_pref <- ggplot(pop_healthcare_facility_data, aes(x = NAME_1)) +
  geom_bar() +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
        plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
        axis.text.x = element_text(size = 5, angle = 45, vjust = 0.8),
        axis.title.x = element_text(face = "bold", family = "georgia", size = 9),
        axis.title.y = element_text(face = "bold", family = "georgia", size = 9)) +
  labs(title = "Number of Healthcare Facilities across Prefectures",
       subtitle = "Hokkaido has the most healthcare facilities, followed by Saitama, Nagano, and Tokyo",
       x = "Prefecture",
       y = "Number of Healthcare Facilities")

ggsave("figures/no_healthcare_pref.png", no_healthcare_pref)

#number of healthcare facilities in each city
distribution_facility_per_city <- ggplot(distinct_healthcare_facilities_per_city, aes(x = n)) + 
  geom_histogram(binwidth = 5) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
        plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
        axis.title.x = element_text(face = "bold", family = "georgia", size = 9),
        axis.title.y = element_text(face = "bold", family = "georgia", size = 9)) +
  labs(title = "Distribution of Healthcare Facilities per City",
       subtitle = "The distribution is unimodal and skewed to the right; most cities have 0~10 facilities",
       x = "Number of Facilities per City",
       y = "Prevalence") +
  xlim(0, 100)

ggsave("figures/distribution_facility_per_city.png", distribution_facility_per_city)

#number of healthcare facilities vs number of elderly people in each prefecture in japan
no_facility_vs_pop <- ggplot(pop_healthcare_facility_data, aes(x = geriatric_count_prefecture, y = facility_count)) +
  geom_point(aes(color = facility_type)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
        plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
        legend.title = element_text(face = "bold", family = "georgia", size = 8),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(face = "bold", family = "georgia", size = 9),
        axis.title.y = element_text(face = "bold", family = "georgia", size = 9)) +
  labs(title = "Number of Citizens over 65 years old in each Prefecture against Number of Facilities",
       subtitle = "There is no clear trend in the relationship between the two variables",
       x = "Number of citizens over 65 years old",
       y = "Facility count",
       col = "Facility Type")

ggsave("figures/no_facility_vs_pop.png", no_facility_vs_pop)  

#percentage of citizens over 65 vs number of healthcare facilities
percentage_no_facility <- ggplot(pop_healthcare_facility_data, aes(x = `2020_geriatric_percentage`, y = facility_count)) +
  geom_point(aes(color = facility_type)) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
        plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
        legend.title = element_text(face = "bold", family = "georgia", size = 8),
        legend.text = element_text(size = 6),
        axis.title.x = element_text(face = "bold", family = "georgia", size = 9),
        axis.title.y = element_text(face = "bold", family = "georgia", size = 9)) +
  labs(title = "Percentage of Citizens over 65 years old in each City against Number of Facilities",
       subtitle = "There seems to be a unimodal trend; the highest number of facilities is found in the 25~30% range",
       x = "Percentage of citizens over 65 years old (per city)",
       y = "Facility count",
       col = "Facility Type")

ggsave("figures/percentage_no_facility.png", percentage_no_facility)  

##all of Japan
density_calc_data <- pop_healthcare_facility_data |> 
  mutate(density = as.numeric(facility_count / `2020_geriatric_percentage`),
         density_pref = as.numeric(facility_count/geriatric_count_prefecture)) 

all_jpn <- ggplot() +
  geom_sf(data = density_calc_data, aes(group = NAME_2, fill = density)) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
        plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
        legend.title = element_text(face = "bold", family = "georgia", size = 8),
        legend.text = element_text(size = 6)) +
  labs(title = "Density Map Viewing the Number of Healthcare Facilities per % of Citizens over 65 years old",
       subtitle = "There is a higher density of healthcare facilities for those over 65 years old in metropolitan areas, such as Tokyo and Osaka",
       fill = "Number of Healthcare Facilities per % of Citizens over 65 years old")

ggsave("figures/all_jpn.png", all_jpn)  

#Kanto Region
kanto <- ggplot() +
  geom_sf(data = new_data, aes(group = NAME_2, fill = density)) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", family = "georgia", size = 12),
        plot.subtitle = element_text(face = "italic", family = "georgia", size = 10),
        legend.title = element_text(face = "bold", family = "georgia", size = 8),
        legend.text = element_text(size = 6)) +
  labs(title = "Density Map Viewing the Number of Healthcare Facilities per % of Citizens over 65 years old (Kanto Region)",
       subtitle = "There is a stark difference between the distribution when comparing Tokyo and its surrounding cities",
       fill = "Number of Healthcare Facilities per % of Citizens over 65 years old") +
  coord_sf(xlim = c(137, 142),
           ylim = c(32, 37))
  
  ggsave("figures/kanto.png", kanto)  
