##################
# Draws quarterly graph of Juvenile TDOs from eMagistrate data
# One line per year, with fiscal quarters on the x axis and counts on the y axis
##################
source("reports/emagistrate_prep.R")
library(dplyr)
library(pander)
library(ggplot2)

TDOJ <- 
  emags %>%
  filter(Type=="TDOJ")

#this is where we start using summary counts per month
TDOJquarterly <- 
  group_by(TDOJ, FQtr, FYear) %>%
  summarise(count = sum(Process.Count))


TDOJquarterly$FQtr <- factor(TDOJquarterly$FQtr)

TDOJquarterly <- 
  group_by(TDOJ, FQtr, FYear) %>%
  summarise(count = sum(Process.Count))

TDOJqp <- 
  ggplot(TDOJquarterly, aes(factor(FQtr), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of ECOs") +
  xlab("Fiscal Quarter")


TDOJqp <- TDOJqp + ylim(0,max(ECOJquarterly$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Fiscal Year")+ ggtitle("Quarterly Frequency of TDOs Issued to Minors, FY2010-FY2015")
print(TDOJqp)
