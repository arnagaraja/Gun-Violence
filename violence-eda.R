library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(ggmap)

df <- read_csv("data/gun-violence-data_01-2013_03-2018.csv")

# Clean the dataset to remove the attributes we don't care about
gun <- df %>% select(-c(incident_url, source_url, incident_url_fields_missing, congressional_district, sources, state_house_district, state_senate_district)) 


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

# Project it onto a map of the US
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

# =====================================================

# READ IN US POPULATION DATA BY STATE
colNames <- c("id1", "id2", "state", "census2010", "cenbase2010", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
pop <- read_csv("data/US\ Population\ by\ State/PEP_2017_PEPANNRES_with_ann.csv", skip = 2, col_names = colNames)

# Only keep the columns and rows we want
pop <- pop %>% select(id1, state, "2014", "2015", "2016", "2017") %>% slice(6:n())

# Casualties by state and year; remove Washington DC from the state rankings, since it will skew the results
casByState <- gun %>% group_by(state, year = format(date, "%Y")) %>% filter(state != "District of Columbia") %>% 
      summarize(n.casualty = sum(n_killed + n_injured), n.injured = sum(n_injured), n.killed = sum(n_killed))

# Add the population and the per capita rate (per 100K people)

# First, generate the population vector:
popList <- numeric()
for (i in 1:nrow(casByState)) {
      popList <- c(popList, as.numeric(pop[pop$state == casByState$state[i], casByState$year[i]]))
}

casByState <- as.data.frame(casByState) %>% mutate(population = popList, cas.per.capita100K = n.casualty/population*100000, k.per.capita100K = n.killed/population*100000, i.per.capita100K = n.injured/population*100000) %>% as_tibble()

# p1: Casualty rate for all years for all states; hard to determine a pattern
g <- ggplot(data = casByState, aes(x = reorder(state, -cas.per.capita100K), y = cas.per.capita100K, fill = year))
p1 <- g + geom_col(position = position_dodge()) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Casualty Rate per 100,000 People", 
           title = "Casualty Rate by State per 100,000 People (2014-2017)")


# Just plot the mean for each year
casByStateMean <- group_by(casByState, state) %>% summarize(meanCasRate = mean(cas.per.capita100K), meanKRate = mean(k.per.capita100K), meanIRate = mean(i.per.capita100K))

g <- ggplot(data = casByStateMean, aes(x = reorder(state, -meanCasRate), y = meanCasRate))
p2 <- g + geom_col(position = position_dodge(), fill = cols[2]) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by State (2014-2017)")

# Add a line for the median
p3 <- p2 + geom_hline(yintercept = median(casByStateMean$meanCasRate), lwd = 1)

# =============================================================================
# READ IN US POPULATION DATA BY CITY
colNames <- c("id1", "id2", "country", "tid1", "tid2", "rank", "geography1", "city", "census2010", "cenbase2010", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
citypop <- read_csv("data/US\ Population\ by\ City/PEP_2016_PEPANNRSIP.US12A_with_ann.csv", skip = 2, col_names = colNames)

# Only keep the columns and rows we want (drop all the "government" and "county" entries)
citypop <- select(citypop, tid2, city, "2014", "2015", "2016") %>% filter(!grepl("[Cc]ounty|government", city))

# Reformatting city names
cityState <- sub("^(.+) (city( \\(balance\\))?|municipality|village|town), (.+)", "\\1, \\4", citypop$city) %>% strsplit(", ")

# split up the city column into city and state columns
cityState <- as.data.frame(matrix(unlist(cityState), nrow = length(cityState), byrow = TRUE), stringsAsFactors = FALSE)
names(cityState) <- c("city", "state")

# Ventura, CA has a nonstandard entry; fix it
cityState$city[grep("\\(", cityState$city)] <- "Ventura"

# Integrate the new columns back to the tibble and rearrange
# Also, since we are missing data for 2017, just reuse the 2016 column
citypop <- mutate(citypop, city = cityState$city, state = cityState$state, "2017" = citypop$`2016`) %>% select(tid2, city, state, "2014", "2015", "2016", "2017")

casByCity <- gun %>% group_by(city_or_county, state, year = format(date, "%Y")) %>% summarize(n.casualty = sum(n_killed + n_injured), n.killed = sum(n_killed), n.injured = sum(n_injured))

# Get a per capita (100K) estimate for each city
# First, generate the population vector:
popList <- numeric()
for (i in 1:nrow(casByCity)) {
      theCity <- casByCity$city_or_county[i]
      theState <- casByCity$state[i]
      theYear <- casByCity$year[i]
      ind <- which(citypop$city == theCity)
      
      if (length(ind) > 0 && theCity == citypop$city[ind] && theState == citypop$state[ind]) {
            popList <- c(popList, as.numeric(citypop[citypop$state ==
                                    theState & citypop$city == theCity, theYear]))
      } else {
            popList <- c(popList, NA)
      }
}

# Get the short name for each state and attach it to the tibble
data(state)
abbs <- data.frame("abb" = state.abb, "state" = state.name, stringsAsFactors = FALSE)
abbs <- rbind(abbs, c("DC", "District of Columbia"), stringsAsFactors = FALSE)
abbList <- unlist(lapply(casByCity$state, function(x) abbs[which(abbs$state == x),]$abb))

meanCBC <- as.data.frame(casByCity) %>% mutate(abb = abbList, population = popList, cas.per.capita100K = n.casualty/population*100000, k.per.capita100K = n.killed/population*100000, i.per.capita100K = n.injured/population*100000) %>% as_tibble()

# Remove NA values and then compute the mean on the remaining values; sort by mean
meanCBC <- group_by(meanCBC, city_or_county, state, abb) %>% filter(!is.na(population)) %>% summarize(meanCasRate = mean(cas.per.capita100K), meanKRate = mean(k.per.capita100K), meanIRate = mean(i.per.capita100K)) %>% arrange(desc(meanCasRate))

top50 <- meanCBC[1:50,]

# Plot it
top50 <- arrange(top50, desc(meanCasRate))
g <- ggplot(data = top50, aes(x = reorder(city_or_county, -meanCasRate), y = meanCasRate))

cityplot <- g + geom_col(position = position_dodge(), fill = cols[2]) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = .95, size = 12)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      guides(fill = FALSE) + 
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by City (2014-2017 where available)")

# Add the state abbreviations to the labels
cityplot <- cityplot + scale_x_discrete(labels = paste(top50$city_or_county, sep = ", ", top50$abb))

# Fatalities
top50 <- arrange(top50, meanKRate)
killplot <- ggplot(data = top50, aes(x = reorder(city_or_county, meanKRate), y = meanKRate)) + geom_col(position = position_dodge(), fill = cols[1]) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = .95, size = 12)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Mean Fatality Rate per 100,000 People", 
           title = "Mean Fatality Rate per 100,000 People by City (2014-2017 where available)") +
      scale_x_discrete(labels = paste(top50$city_or_county, sep = ", ", top50$abb)) + 
      coord_flip()

# Fatalities and Injuries
library(reshape2)
top50 <- arrange(top50, desc(meanCasRate))
top50melt <- melt(top50[,c("city_or_county", "abb", "meanKRate", "meanIRate")], id.vars = c("city_or_county", "abb"))

kiplot <- ggplot(data = top50melt) + geom_col(aes(x = city_or_county, y = value, fill = variable)) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = .95, size = 12)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      theme(legend.title = element_blank()) + 
      scale_fill_discrete(labels = c("Fatalities", "Injuries")) +
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by City (2014-2017 where available)") +
      scale_x_discrete(limits = top50$city_or_county, labels = paste(top50$city_or_county, sep = ", ", top50$abb)) +
      theme(legend.text = element_text(size = 16))


# Adhoc symmetric horizontal bar plot
top50 <- arrange(top50, meanCasRate)
top50 <- mutate(top50, meanIRate = -meanIRate)
top50melt2 <- melt(top50[,c("city_or_county", "abb", "meanKRate", "meanIRate")], id.vars = c("city_or_county", "abb"))

kiplot2 <- ggplot(data = top50melt2) + geom_col(aes(x = city_or_county, y = value, fill = variable)) +
      scale_y_continuous(breaks = c(50, 0, -50, -100, -150), labels = c("50", "0", "50", "100", "150")) + 
      theme(axis.text.x  = element_text(hjust = .95, size = 12)) + 
      theme(axis.text.y = element_text(vjust = 0.5, size = 10)) +
      theme(axis.title.y = element_text(size = 14)) + 
      theme(plot.title = element_text(size = 16)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.position = "bottom") + 
      theme(legend.text = element_text(size = 14)) + 
      scale_fill_discrete(labels = c("Fatalities", "Injuries"), guide = guide_legend(reverse = TRUE)) +
      labs(x = NULL) +
      labs(y = "Mean Injury/Fatality Rate per 100,000 People") +
      labs(title = "Mean Injury/Fatality Rate per 100,000 People by City (2014-2017 where available)") + 
      scale_x_discrete(limits = top50$city_or_county, labels = paste(top50$city_or_county, sep = ", ", top50$abb)) +
      coord_flip()

library(plotly)
ggplotly(kiplot2)

# To Do:
# Use plotly to show value when hovering over the bar
# Classify each state based on strictness of gun laws
      