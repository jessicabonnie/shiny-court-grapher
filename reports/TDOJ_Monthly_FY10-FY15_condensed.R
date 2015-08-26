
# Load libraries
library(dplyr)
library(stringr)
library(ggplot2)

# read in emagistrate data (emags)
source("reports/emagistrate_prep.R")

TDOJ <- 
  emags %>%
  filter(Type=="TDOJ")


#this is where we start using summary counts per month (FYMonthAbbrev factor is ordered by Fiscal Calendar)
TDOJmonthly <- 
  group_by(TDOJ, FYear, Month, FYMonthAbbrev) %>%
  summarise(count = sum(Process.Count))

# Some years are similar and will be averaged
TDOJmonthly$YearCat <- cut(as.numeric(TDOJmonthly$FYear), breaks=c(2009,2011, 2013, 2014,2015), labels=c("FY2010/FY2011","FY2012/FY2013", "FY2014", "FY2015"))

TDOJmonthlyCondensed <- TDOJmonthly %>%
  group_by(YearCat, FYMonthAbbrev) %>%
  summarise(count = mean(count)) 

# Draw Monthly Frequency Plot
condensed.plot <- 
  ggplot(TDOJmonthlyCondensed, aes(factor(FYMonthAbbrev), count, group=factor(YearCat), color=factor(YearCat))) + 
  geom_line() +
  ylab("Number of TDOs") +
  xlab("Month")

condensed.plot + ylim(0,max(TDOJmonthly$count)) + geom_line(size=1.2) + geom_point(aes(shape=factor(YearCat)), size=3) + scale_colour_discrete(name  ="Year") + scale_shape_discrete(name="Year") + ggtitle("Monthly Frequency of TDOs Issued to Minors, FY2010-FY2015")

