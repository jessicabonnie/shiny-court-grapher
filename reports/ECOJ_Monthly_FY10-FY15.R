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
ECOJmonthly2 <- 
  group_by(ECOJ, month_id, FYear, Month) %>%
  summarise(count = sum(Process.Count))

ECOJmonthly2$monthGraph <- factor(ECOJmonthly2$Month, levels=c("07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05", "06"))


p <- 
  ggplot(ECOJmonthly2, aes(factor(monthGraph), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of ECOs") +
  xlab("Month")
p

p + ylim(0,max(ECOJmonthly2$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Year")+ ggtitle("Monthly Frequency of ECOs Issued to Minors, FY2009-FY2015")
