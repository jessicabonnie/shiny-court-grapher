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

g <- ggplot(TDOJmonthly, aes(x=month_id, y=count)) + 
  geom_bar(stat="identity")

g

q <- ggplot(TDOJ, aes(factor(Month), fill=factor(Year))) + geom_bar(stat="bin", position="dodge")
q

n <- ggplot(TDOJ, aes(x=factor(Year), y=sum(Process.Count), fill=factor(Month))) + geom_point()
n

#this is where we start using summary counts per month
TDOJmonthly2 <- 
  group_by(TDOJ, month_id, FYear, Month) %>%
  summarise(count = sum(Process.Count))

TDOJmonthly2$monthGraph <- factor(TDOJmonthly2$Month, levels=c("07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05", "06"))

p <- 
  ggplot(TDOJmonthly2, aes(factor(monthGraph), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of TDOs") +
  xlab("Month")
p

p + ylim(0,max(TDOJmonthly2$count)) + geom_line(size=1.2) + 
  scale_colour_discrete(name  ="Fiscal Year") +
  ggtitle("Monthly Frequency of TDOs Issued to Minors, FY2010-FY2015")
