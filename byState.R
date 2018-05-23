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

casByState <- ungroup(casByState) %>% mutate(population = popList, cas.per.capita100K = n.casualty/population*100000, k.per.capita100K = n.killed/population*100000, i.per.capita100K = n.injured/population*100000)

# p1: Casualty rate for all years for all states; hard to determine a pattern
g <- ggplot(data = casByState, aes(x = reorder(state, -cas.per.capita100K), y = cas.per.capita100K, fill = year))
p1 <- g + geom_col(position = position_dodge()) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Casualty Rate per 100,000 People", 
           title = "Casualty Rate by State per 100,000 People (2014-2017)")


# Just plot the mean for each year
meanCBS <- group_by(casByState, state) %>% summarize(meanCasRate = mean(cas.per.capita100K), meanKRate = mean(k.per.capita100K), meanIRate = mean(i.per.capita100K))

g <- ggplot(data = meanCBS, aes(x = reorder(state, -meanCasRate), y = meanCasRate))
p2 <- g + geom_col(position = position_dodge(), fill = cols[2]) + 
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size = 14)) + 
      theme(axis.title.y = element_text(size = 16)) + 
      theme(plot.title = element_text(size = 16)) + 
      labs(x = NULL, y = "Mean Casualty Rate per 100,000 People", 
           title = "Mean Casualty Rate per 100,000 People by State (2014-2017)")

# Add a line for the median
p3 <- p2 + geom_hline(yintercept = median(casByStateMean$meanCasRate), lwd = 1)