# Classify each state based on strictness of gun laws
library(readr)
library(dplyr)

lawsdf <- read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/48_states_1991_data.csv")

lawsdf <- rbind(lawsdf, read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/Hawaii_1991_data.csv"))

lawsdf <- rbind(lawsdf, read_csv("~/R Projects/Gun Violence/Gun-Violence/data/Laws/Alaska_1991_data.csv"))

# Rename one of the columns to something easy to reference
lawsdf <- rename(lawsdf, law = `Law? 0/1`)

laws.by.year <- group_by(lawsdf, State, Year) %>% summarize(laws = sum(law))


# Grab the years we are interested in, 2014-2017, and take the average number of laws for the period
mean.laws.by.year <- filter(laws.by.year, Year >= 2014) %>% summarize(meanlaw = mean(law))

# Histogram of number of laws
ggplot(data = mean.laws.by.year, aes(x = meanlaw)) + geom_histogram(binwidth = 10, col = "black") + labs(x = "Mean Number of Laws, 2014-2017", y = "Frequency")

# Is there a correlation between the number of laws and the number of casualties?
casByStateMean <- arrange(casByStateMean, state)
mean.laws.by.year <- arrange(mean.laws.by.year, State)
casByStateMean <- casByStateMean %>% mutate(numlaws = mean.laws.by.year$meanlaw)
qplot(x = numlaws, y = meanCasRate, data = casByStateMean, xlab = "Number of Laws", ylab = "Casualty Rate")


# PLOT THE CASUALTIES BY STATE AND COLOR THE BARS BASED ON THE NUMBER OF LAWS

### N.B. - MAKE SURE THE NECESSARY DATASETS ARE LOADED INTO THE ENVIRONMENT
g <- ggplot(data = casByStateMean, aes(x = reorder(state, -meanCasRate), y = meanCasRate, fill = numlaws))
p2 <- g + geom_col(color = "gray") + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by State (2014-2017)")

pcaswithlaw <- p2 + scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue"), midpoint = max(casByStateMean$numlaws)/2, name = "Avg. Number of Gun Laws", guide = guide_colorbar(direction = "horizontal", title.position = "top", barwidth = 10, title.hjust = .5)) + 
      theme(legend.background = element_rect(fill="gray90", size=.3, color = "black", linetype = 1)) +
      theme(legend.justification=c(1,1), legend.position=c(1,1)) +
      geom_hline(yintercept = median(casByStateMean$meanCasRate), lwd = 1)
pcaswithlaw


