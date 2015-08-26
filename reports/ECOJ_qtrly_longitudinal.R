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

ECOJquarterlyLong<-
  unite(ECOJquarterly, Fyear_FQtr, FYear, FQtr, sep="-")

p <- 
  ggplot(ECOJquarterlyLong, aes(x=Fyear_FQtr, y=count, group=1)) + geom_line() +
  ylab("Number of ECOs") +
  xlab("Fiscal Quarter")
p

p + ylim(0,max(ECOJquarterlyLong$count)) + geom_line(size=1.2) + ggtitle("ECO Trends (Minors Only), FY2010-FY2015") +
  scale_x_discrete(labels=c("10-1", "10-2", "10-3", "10-4", "11-1", "11-2", "11-3", "11-4",
                            "12-1", "12-2", "12-3", "12-4", "13-1", "13-2", "13-3", "13-4",
                            "14-1", "14-2", "14-3", "14-4", "15-1", "15-2","15-3", "15-4"))
