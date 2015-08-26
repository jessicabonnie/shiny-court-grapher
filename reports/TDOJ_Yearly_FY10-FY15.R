# read in emagistrate data
emags <- read.csv("CopyOfECOTDOFips08_14.txt")

emags$FYear<- emags$Year
emags[emags$Month > 6, ]$FYear <- as.numeric(emags[emags$Month > 6, ]$Year) + 1


library(dplyr)
library(stringr)

TDOJ <- 
  emags %>%
  filter(Type=="TDOJ")

TDOJ$Month <- str_pad(as.character(TDOJ$Month), 2, side="left", pad="0")


TDOJ$month_id <- factor(paste(TDOJ$FYear, as.character(TDOJ$Month), sep=" "))

TDOJmonthly <- 
  group_by(TDOJ, month_id) %>%
  summarise(count = n())

TDOJmonthlybyFIPS <- 
  group_by(TDOJ, Fips.Code, month_id) %>%
  summarise(count = n())

TDOJmonthlybyFIPS


g <- ggplot(TDOJmonthly, aes(x=month_id, y=count)) + 
  geom_bar(stat="identity")

g

q <- ggplot(TDOJ, aes(factor(Month), fill=factor(Year))) + geom_bar(stat="bin", position="dodge")
q

n <- ggplot(TDOJ, aes(x=factor(Year), y=sum(Process.Count), fill=factor(Month))) + geom_point()
n

#this is where we start using summary counts per month
TDOJyearly <- 
  group_by(TDOJ, FYear) %>%
  summarise(count = sum(Process.Count))

p <-  ggplot(TDOJyearly, aes(x=FYear, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2.5)) + ylab("Number of TDOs") +  xlab("Fiscal Year")

p

p + ylim(1250,max(TDOJyearly$count)) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Annual Frequency of TDOs Issued to Minors, FY2010-FY2015")
