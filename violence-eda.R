library(dplyr)
#library(ggplot2)
#library(reshape2)
library(readr)
#library(gridExtra)

df <- read_csv("data/gun-violence-data_01-2013_03-2018.csv")

# Clean the dataset to keep the rows we want and remove anything from 2018 since the year is not complete
gun <- df %>% select(incident_id, date, state, city_or_county, address, n_killed, n_injured, gun_stolen, gun_type, incident_characteristics, latitude, location_description, longitude, n_guns_involved, notes, participant_age, participant_age_group, participant_gender, participant_name, participant_relationship, participant_status, participant_type) %>% filter(format(date, "%Y") != 2018 & format(date, "%Y") != 2013)

# Remove the suicides (excluding murder/suicides)
suicide <- filter(gun, grepl("\\|\\|Suicide\\^", incident_characteristics)
                      & n_killed + n_injured < 2)

gun <- filter(gun, !(incident_id %in% suicide$incident_id))

### NOTE: DO WE NEED TO DECREASE N_KILLED FOR THE MURDER/SUICIDES BY 1? ###

# Gun Crime
gun %>% mutate(n_casualty = n_killed + n_injured) %>% group_by(state) %>% summarize(n_casualty = sum(n_casualty)) %>% ggplot(aes(x = reorder(state, -n_casualty), y = n_casualty)) + geom_col() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))