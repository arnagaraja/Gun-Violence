# Classify each state based on strictness of gun laws
library(readr)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

lawsdf <- read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/48_states_1991_data.csv")
lawsdf <- rbind(lawsdf, read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/Hawaii_1991_data.csv"))
lawsdf <- rbind(lawsdf, read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/Alaska_1991_data.csv"))

# Rename one of the columns to something easy to reference
lawsdf <- rename(lawsdf, law = `Law? 0/1`)

laws.by.year <- group_by(lawsdf, State, Year) %>% summarize(laws = sum(law))


# Grab the years we are interested in, 2014-2017, and take the average number of laws for the period
mean.laws.by.year <- filter(laws.by.year, Year >= 2014) %>% summarize(meanlaw = mean(laws))

# Histogram of number of laws
ggplot(data = mean.laws.by.year, aes(x = meanlaw)) + geom_histogram(binwidth = 10, col = "black") + labs(x = "Mean Number of Laws, 2014-2017", y = "Frequency")

# Is there a correlation between the number of laws and the number of casualties?
meanCBS <- arrange(meanCBS, state)
mean.laws.by.year <- arrange(mean.laws.by.year, State)
meanCBS <- mutate(meanCBS, numlaws = mean.laws.by.year$meanlaw)
qplot(x = numlaws, y = meanCasRate, data = meanCBS, xlab = "Number of Laws", ylab = "Casualty Rate")


# PLOT THE CASUALTIES BY STATE AND COLOR THE BARS BASED ON THE NUMBER OF LAWS

### N.B. - MAKE SURE THE NECESSARY DATASETS ARE LOADED INTO THE ENVIRONMENT
g <- ggplot(data = meanCBS, aes(x = reorder(state, -meanCasRate), y = meanCasRate, fill = numlaws))
p2 <- g + geom_col(color = "gray") + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by State (2014-2017)")

pcaswithlaw <- p2 + scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue"), midpoint = max(casByStateMean$numlaws)/2, name = "Avg. Number of Gun Laws", guide = guide_colorbar(direction = "horizontal", title.position = "top", barwidth = 10, title.hjust = .5)) + 
      theme(legend.background = element_rect(fill="gray90", size=.3, color = "black", linetype = 1)) +
      theme(legend.justification=c(1,1), legend.position=c(1,1)) +
      geom_hline(yintercept = median(meanCBS$meanCasRate), lwd = 1)
pcaswithlaw


### NOW DO IT WITH THE CITY DATA!
### N.B. - MAKE SURE THE NECESSARY DATASETS ARE LOADED INTO THE ENVIRONMENT

# Is there a correlation between the number of state laws and the number of casualties in each city? First, remove DC since we don't have data on the number of gun laws there
meanCBClaws <- filter(meanCBC, state != "District of Columbia")

lawlist <- sapply(meanCBClaws$state, function(x) mean.laws.by.year[mean.laws.by.year$State == x,2])

meanCBClaws <- ungroup(meanCBClaws) %>% mutate("numlaws" = round(unlist(lawlist), digits = 1), meanCasRate = round(meanCasRate, digits = 1), meanKRate = round(meanKRate, digits = 1), meanIRate = round(meanIRate, digits = 1))

oldnames <- names(meanCBClaws)

# Do some renaming to make things easier to understand
names(meanCBClaws) <- c("City", "state.long", "State", "Casualty Rate", "Fatality Rate", "Injury Rate", "Number of Laws")

p.allcities <- ggplot(data = meanCBClaws, aes(x = `Number of Laws`, y = `Casualty Rate`, color = State)) +
      geom_point(aes(text = paste(City, ", ", State, "<br>Number of Laws: ", `Number of Laws`, "<br>Casualty Rate: ", `Casualty Rate`, "<br>Fatality Rate: ", `Fatality Rate`, "<br>Injury Rate: ", `Injury Rate`, sep = ""))) + 
      labs(x = "Number of Laws") + 
      labs (y = "Casualty Rate (Per 100,000 People)") +
      theme(legend.position = "none")

ggplotly(p.allcities, tooltip = c("text"))


bw.allcities <- ggplot(data = meanCBClaws, aes(x = state.long, y = `Casualty Rate`, fill = `Number of Laws`)) +
      geom_boxplot() + 
      labs(x = NULL) + 
      labs (y = "Casualty Rate (Per 100,000 People)") +
      scale_fill_gradient2(midpoint = max(meanCBClaws$`Number of Laws`)/2, name = "Avg. Number of State Gun Laws", guide = guide_colorbar(direction = "horizontal", title.position = "top", barwidth = 10, title.hjust = .5)) + 
      theme(legend.background = element_rect(fill="gray90", size=.3, color = "black", linetype = 1)) +
      theme(legend.justification=c(1,1), legend.position=c(1,1)) +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16))

bw.allcities




# Symmetric horizontal bar plot
names(meanCBClaws) <- oldnames # Set the names back to what they were so I don't have to rewrite the code
top49 <- meanCBClaws[1:49,] %>% arrange(meanCasRate) %>% mutate(meanIRate = -meanIRate)
top49melt <- melt(top49[,c("city_or_county", "abb", "meanKRate", "meanIRate", "numlaws")], id.vars = c("city_or_county", "abb", "numlaws"))

kiplot3 <- ggplot(data = top49melt) + geom_col(aes(x = city_or_county, y = value, fill = numlaws)) +
      scale_y_continuous(breaks = c(50, 0, -50, -100, -150), labels = c("50", "0", "50", "100", "150")) + 
      theme(axis.text.x  = element_text(hjust = .95, size = 12)) + 
      theme(axis.text.y = element_text(vjust = 0.5, size = 10)) +
      theme(axis.title.y = element_text(size = 14)) + 
      theme(plot.title = element_text(size = 16)) + 
      #theme(legend.title = element_blank()) + 
      #theme(legend.position = "bottom") + 
      #theme(legend.text = element_text(size = 14)) + 
      labs(x = NULL, y = NULL) +
      labs(title = "Mean Injury/Fatality Rate per 100,000 People by City (2014-2017 where available)") + 
      scale_x_discrete(limits = top49$city_or_county, labels = paste(top49$city_or_county, sep = ", ", top49$abb)) +
      geom_hline(yintercept = 0, lwd = 2, color = "black") +
      coord_flip()

citycaswithlaw <- kiplot3 + scale_fill_gradient2(midpoint = max(top49$numlaws)/2, name = "Avg. Number of State Gun Laws", guide = guide_colorbar(direction = "horizontal", title.position = "top", barwidth = 10, title.hjust = .5)) + 
      theme(legend.background = element_rect(fill="gray90", size=.3, color = "black", linetype = 1)) +
      theme(legend.justification=c(.125,.125), legend.position=c(.125,.125)) +
      annotate("label", x = 25, y = -100, label = "Injuries", size = 6) + 
      annotate("label", x = 25, y = 45, label = "Fatalities", size = 6)

citycaswithlaw
