##################
# Draws monthly table of Juvenile Emergency Custody Orders from eMagistrate data
# One line per year, with fiscal months on the x axis and counts on the y axis
##################
# Load libraries
#library(dplyr)
#library(stringr)
#library(ggplot2)

# read in emagistrate data (emags)
#source("reports/emagistrate_prep.R")

ECOJ <- 
  emags %>%
  filter(Type=="ECOJ")

#ECOJ$Month <- str_pad(as.character(ECOJ$Month), 2, side="left", pad="0")


#this is where we start using summary counts per month
ECOJmonthly <- 
  group_by(ECOJ, FYear, Month, FYMonthAbbrev) %>%
  summarise(count = sum(Process.Count))

# Some years are similar and will be averaged
ECOJmonthly$YearCat <- cut(as.numeric(ECOJmonthly$FYear), breaks=c(2009,2010, 2011, 2013, 2014,2015), labels=c("FY2010","FY2011","FY2012/FY2013", "FY2014", "FY2015"))

ECOJmonthlyCondensed <- ECOJmonthly %>%
  group_by(YearCat, FYMonthAbbrev) %>%
  summarise(count = mean(count)) 

# Draw Monthly Frequency Plot
condensed.plot <- 
  ggplot(ECOJmonthlyCondensed, aes(factor(FYMonthAbbrev), count, group=factor(YearCat), color=factor(YearCat))) + 
  geom_line() +
  ylab("Number of ECOs") +
  xlab("Month")

condensed.plot + ylim(0,max(ECOJmonthly$count)) + geom_line(size=1.2) + geom_point(aes(shape=factor(YearCat)), size=3) + scale_colour_discrete(name  ="Year") + scale_shape_discrete(name="Year") + ggtitle("Monthly Frequency of ECOs Issued to Minors, FY2010-FY2015")
