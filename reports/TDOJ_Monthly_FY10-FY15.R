
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

# Draw Monthly Frequency Plot
plot <- 
  ggplot(TDOJmonthly, aes(factor(FYMonthAbbrev), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of TDOs") +
  xlab("Month")

plot + ylim(0,max(TDOJmonthly$count)) + geom_line(size=1.2) + 
  scale_colour_discrete(name  ="Fiscal Year") +
  ggtitle("Monthly Frequency of TDOs Issued to Minors, FY2010-FY2015")
