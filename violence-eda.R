library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)

df <- read_csv("data/gun-violence-data_01-2013_03-2018.csv")

# Clean the dataset to remove the attributes we don't care about
gun <- df %>% select(-c(incident_url, source_url, incident_url_fields_missing, congressional_district, sources, state_house_district, state_senate_district)) 


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
## No, it probably won't make a difference ##



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

# ======================================
statemap <- map_data("state")
no_axes <- theme_bw() + theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
)
p1 <- ggplot(data = statemap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_fixed(1.3) + no_axes
p2 <- p1 + geom_point(data = gun, aes(x = longitude, y = latitude, col = n_killed, size = 0.001, alpha = .1))