library(dplyr)
library(tidyr)
library(pander)

emags <- read.csv("CopyOfECOTDOFips08_14.txt")

emags$FYear<- emags$Year
emags[emags$Month > 6, ]$FYear <- as.numeric(emags[emags$Month > 6, ]$Year) + 1

emagsECOMonthly <- emags
  
  emagsECOMonthly$monthGraph <- factor(emagsECOMonthly$Month, levels=c("7", "8", "9", "10", "11", "12", "1", "2", "3", "4", "5", "6"))

  emagsECOMonthly<-filter(emagsECOMonthly, Type=="ECOJ")
  
emagsECOMonthly <- group_by(emagsECOMonthly, FYear, monthGraph) %>%
  summarise(count = sum(Process.Count)) %>%
  spread(FYear, count)

pander(emagsECOMonthly, caption = "Monthly Frequency of ECOs Issued to Minors, FY2010-FY2015")


