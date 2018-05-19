library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(ggmap)

df <- read_csv("data/gun-violence-data_01-2013_03-2018.csv")

# Clean the dataset to remove the attributes we don't care about
gun <- df %>% select(-c(incident_url, source_url, incident_url_fields_missing, congressional_district, sources, state_house_district, state_senate_district))

# INITIAL EXPLORATION

# How many gun casualties (killed + injured) each year? 
gun %>% group_by(Year = format(date, "%Y")) %>% summarize(n = sum(n_injured + n_killed)) %>% ggplot(aes(x = Year, y = n)) + geom_col()

# Clearly there are missing data from 2013, and 2018 isn't complete yet. Let's drop these two years
gun <- filter(gun, format(date, "%Y") != 2018 & format(date, "%Y") != 2013)

# Let's also remove the suicides (excluding murder/suicides)
suicide <- filter(gun, grepl("\\|\\|Suicide\\^", incident_characteristics)
                  & n_killed + n_injured < 2)

gun <- filter(gun, !(incident_id %in% suicide$incident_id))

### NOTE: DO WE NEED TO DECREASE N_KILLED FOR THE MURDER/SUICIDES BY 1? ###
## No, the difference will be unnoticeable ##