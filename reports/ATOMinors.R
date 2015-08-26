library(dplyr)
library(tidyr)
library(pander)

ATO <- read.csv("ATO with TimeInfo.txt")

ATO<-filter(ATO, Form=="ECOJ"| Form=="TDOJ")

ATO_ECO <- 
  ATO %>%
  filter(Form=="ECOJ") %>%
  group_by(Month, FYEAR) %>%
  summarise(count = n())

names(ATO_ECO)[names(ATO_ECO)=="FYEAR"] <- "Fiscal Year"
names(ATO_ECO)[names(ATO_ECO)=="count"] <- "# of ATOs Issued for Minors under an ECO"

pander(ATO_ECO, caption = "Monthly Frequency of ATOs Issued for Minors under an ECO, FY10-FY15")

#what is a good way to display data for these things that are so uncommon in minors?


  ATO_Tot<- ATO %>%
  group_by(FYEAR, Form) %>%
  summarise(count = n()) %>%
  spread(FYEAR, count)

names(ATO_ECO)[names(ATO_ECO)=="FYEAR"] <- "Fiscal Year"
names(ATO_ECO)[names(ATO_ECO)=="count"] <- "# of ATOs Issued for Minors under an ECO"

pander(ATO_Tot, caption = "Annual Frequency of ATOs Issued for Minors under an ECO, FY10-FY15")
