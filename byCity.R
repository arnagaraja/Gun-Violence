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

# In the original gun violence data, the cities listed for New York are split into the five boroughs. So, for example, Brooklyn and The Bronx are separate entries, when they need to be considered "New York" for our purposes. Let's fix that manually.
ny <- filter(gun, state == "New York")
nycInd <- grep("Manhattan|Queens|Bronx|Staten Island|Brooklyn|New York City", ny$city_or_county)
nyc <- ny[nycInd,] %>% filter(city_or_county != "Queensbury")

# Now go through the gun dataframe and change all relevant city_or_state entries to "New York"
for(i in 1:nrow(nyc)) {
      incident <- nyc$incident_id[i]
      index <- which(gun$incident_id == incident)
      gun[index,"city_or_county"] <- "New York"
}

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
      ind <- which(citypop$city == theCity & citypop$state == theState)
      
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
meanCBC <- filter(meanCBC, !is.na(population)) %>% 
      group_by(city_or_county, state, abb) %>% 
      summarize(meanCasRate = mean(cas.per.capita100K), meanKRate = mean(k.per.capita100K), meanIRate = mean(i.per.capita100K)) %>% 
      arrange(desc(meanCasRate))

top50 <- meanCBC[1:50,]

# Plot it
top50 <- arrange(top50, desc(meanCasRate))

cityplot <- ggplot(data = top50, aes(x = reorder(paste(city_or_county, sep = ", ", top50$abb), -meanCasRate), y = meanCasRate)) + 
      geom_col(position = position_dodge(), fill = cols[2]) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = .95, size = 12)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      guides(fill = FALSE) + 
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by City (2014-2017 where available)") +
      scale_x_discrete(labels = paste(top50$city_or_county, sep = ", ", top50$abb))

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
top50 <- mutate(top50, meanIRate = -meanIRate, city = paste(city_or_county, sep = ", ", abb))
top50melt2 <- melt(top50[,c("city", "meanKRate", "meanIRate", "meanCasRate")], id.vars = c("city", "meanCasRate"))

kiplot2 <- ggplot(data = top50melt2) + geom_col(aes(x = reorder(city, meanCasRate), y = value, fill = variable)) +
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
      #scale_x_discrete(limits = top50$city_or_county, labels = ) +
      coord_flip()

#library(plotly)
#ggplotly(kiplot2)