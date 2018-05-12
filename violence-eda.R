library(dplyr)
#library(ggplot2)
#library(reshape2)
library(readr)
#library(gridExtra)
library(RColorBrewer)


df <- read_csv("data/gun-violence-data_01-2013_03-2018.csv")

# Clean the dataset to keep the attributes that we are interested in
gun <- df %>% select(incident_id, date, state, city_or_county, address, n_killed, n_injured, gun_stolen, gun_type, incident_characteristics, latitude, location_description, longitude, n_guns_involved, notes, participant_age, participant_age_group, participant_gender, participant_name, participant_relationship, participant_status, participant_type) 


# How many gun casualties (killed + injured) each year? 
#gun <- mutate(gun, n_casualty = n_killed + n_injured)
gun %>% group_by(Year = format(date, "%Y")) %>% summarize(n = sum(n_injured + n_killed)) %>% ggplot(aes(x = Year, y = n)) + geom_col()

# Clearly there are missing data from 2013, and 2018 isn't complete yet. Let's drop these two years
gun <- filter(gun, format(date, "%Y") != 2018 & format(date, "%Y") != 2013)

# Let's also remove the suicides (excluding murder/suicides)
suicide <- filter(gun, grepl("\\|\\|Suicide\\^", incident_characteristics)
                      & n_killed + n_injured < 2)

gun <- filter(gun, !(incident_id %in% suicide$incident_id))

### NOTE: DO WE NEED TO DECREASE N_KILLED FOR THE MURDER/SUICIDES BY 1? ###



# Gun Casualties by State

cols <- brewer.pal(3, "Dark2")

gun %>% group_by(state) %>% 
      summarize(n_casualty = sum(n_killed + n_injured)) %>% 
      ggplot(aes(x = reorder(state, -n_casualty), y = n_casualty)) + 
      geom_col(fill = cols[1]) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Casualty (Injured + Killed)", 
           title = "Number of Gun Casualties in the US by State for 2014-2017")