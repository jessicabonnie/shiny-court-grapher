# read in emagistrate data
emags <- read.csv("CopyOfECOTDOFips08_14.txt")

emags$FYear<- emags$Year
emags[emags$Month > 6, ]$FYear <- as.numeric(emags[emags$Month > 6, ]$Year) + 1


library(dplyr)
library(stringr)

ECOJ <- 
  emags %>%
  filter(Type=="ECOJ")

ECOJ$Month <- str_pad(as.character(ECOJ$Month), 2, side="left", pad="0")


ECOJ$month_id <- factor(paste(ECOJ$FYear, as.character(ECOJ$Month), sep=" "))

ECOJmonthly <- 
  group_by(ECOJ, month_id) %>%
  summarise(count = n())

ECOJmonthlybyFIPS <- 
  group_by(ECOJ, Fips.Code, month_id) %>%
  summarise(count = n())

ECOJmonthlybyFIPS


g <- ggplot(ECOJmonthly, aes(x=month_id, y=count)) + 
  geom_bar(stat="identity")

g

q <- ggplot(ECOJ, aes(factor(Month), fill=factor(Year))) + geom_bar(stat="bin", position="dodge")
q

n <- ggplot(ECOJ, aes(x=factor(Year), y=sum(Process.Count), fill=factor(Month))) + geom_point()
n

#this is where we start using summary counts per month
ECOJyearly <- 
  group_by(ECOJ, FYear) %>%
  summarise(count = sum(Process.Count))

p <-  ggplot(ECOJyearly, aes(x=FYear, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2)) + ylab("Number of ECOs") +  xlab("Fiscal Year")

p

p + ylim(0,max(ECOJyearly$count)) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Annual Frequency of ECOs Issued to Minors, FY2010-FY2015")
