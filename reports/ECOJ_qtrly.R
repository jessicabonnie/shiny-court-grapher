emags <- read.csv("ECOTDOFips08_14.txt")
library(dplyr)
library(stringr)
library(tidyr)
library(pander)
library(ggplot2)

emags$FYear<- emags$Year
emags[emags$Month > 6, ]$FYear <- as.numeric(emags[emags$Month > 6, ]$Year) + 1

emags$FQtr <- emags$Month
emags[emags$Month == 7|emags$Month==8| emags$Month==9, ]$FQtr <- 1
emags[emags$Month == 10|emags$Month==11| emags$Month==12, ]$FQtr <- 2
emags[emags$Month == 1|emags$Month==2| emags$Month==3, ]$FQtr <- 3
emags[emags$Month == 4|emags$Month==5| emags$Month==6, ]$FQtr <- 4

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

p <- 
  ggplot(ECOJquarterly, aes(factor(FQtr), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of ECOs") +
  xlab("Fiscal Quarter")
p

p + ylim(0,max(ECOJquarterly$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Fiscal Year")+ ggtitle("Quarterly Frequency of ECOs Issued to Minors, FY2010-FY2015")
