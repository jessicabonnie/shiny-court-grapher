##################
# Draws quarterly graph of Juvenile ECOs from eMagistrate data
# One line per year, with fiscal quarters on the x axis and counts on the y axis
##################
source("reports/emagistrate_prep.R")
library(dplyr)
library(pander)
library(ggplot2)

ECOJ <- 
  emags %>%
  filter(Type=="ECOJ")

#this is where we start using summary counts per month
ECOJquarterly <- 
  group_by(ECOJ, FQtr, FYear) %>%
  summarise(count = sum(Process.Count))


ECOJquarterly$FQtr <- factor(ECOJquarterly$FQtr)

ECOJquarterly <- 
  group_by(ECOJ, FQtr, FYear) %>%
  summarise(count = sum(Process.Count))

ECOJqp <- 
  ggplot(ECOJquarterly, aes(factor(FQtr), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of ECOs") +
  xlab("Fiscal Quarter")


ECOJqp <- ECOJqp + ylim(0,max(ECOJquarterly$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Fiscal Year")+ ggtitle("Quarterly Frequency of ECOs Issued to Minors, FY2010-FY2015")
print(ECOJqp)
