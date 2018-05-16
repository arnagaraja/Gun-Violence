library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(ggmap)

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
p1 <- ggplot(data = statemap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_fixed(1.3, xlim = c(-130,-60), ylim = c(20,50))
p2 <- p1 + geom_point(data = gun, aes(x = longitude, y = latitude, col = n_killed), size = 0.001, alpha = .1, inherit.aes = FALSE) + theme_void() + theme(legend.position = "none")
#p2

png("p2.png", width = 2056, height = 2056)
p2
dev.off()


# READ IN US POPULATION DATA BY STATE
colNames <- c("id1", "id2", "state", "census2010", "cenbase2010", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
pop <- read_csv("data/US\ Population\ by\ State/PEP_2017_PEPANNRES_with_ann.csv", skip = 2, col_names = colNames)

# Only keep the columns and rows we want
pop <- pop %>% select(id1, state, "2014", "2015", "2016", "2017") %>% slice(6:n())

# Casualties by state and year; remove Washington DC from the state rankings, since it will skew the results
casByState <- gun %>% group_by(state, year = format(date, "%Y")) %>% filter(state != "District of Columbia") %>% 
      summarize(n.casualty = sum(n_killed + n_injured))



# Add the population and the per capita rate (per 100K people)

# First, generate the population vector:
popList <- numeric()
for (i in 1:nrow(casByState)) {
      popList <- c(popList, as.numeric(pop[pop$state == casByState$state[i], casByState$year[i]]))
}

casByState <- as.data.frame(casByState) %>% mutate(population = popList, per.capita100K = n.casualty/population*100000) %>% as_tibble()

# p1: Casualty rate for all years for all states; hard to determine a pattern
g <- ggplot(data = casByState, aes(x = reorder(state, -per.capita100K), y = per.capita100K, fill = year))
p1 <- g + geom_col(position = position_dodge()) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Casualty Rate per 100,000 People", 
           title = "Casualty Rate by State per 100,000 People (2014-2017)")


# Just plot the mean for each year
casByStateMean <- group_by(casByState, state) %>% summarize(meanCasRate = mean(per.capita100K))

g <- ggplot(data = casByStateMean, aes(x = reorder(state, -meanCasRate), y = meanCasRate))
p2 <- g + geom_col(position = position_dodge(), fill = cols[2]) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by State (2014-2017)")

# Add a line for the median
p3 <- p2 + geom_hline(yintercept = median(casByStateMean$meanCasRate), lwd = 1)

# To Do:
# Use plotly to show value when hovering over the bar
# Classify each state based on strictness of gun laws
# Look at cities