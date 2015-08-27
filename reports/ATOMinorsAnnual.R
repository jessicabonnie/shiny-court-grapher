library(dplyr)
library(tidyr)
library(pander)

ATO <- read.csv("data/ATO with TimeInfo_8_26_15.txt")

ATO<-filter(ATO, Form=="ECOJ"| Form=="TDOJ")

  ATO_Tot<- ATO %>%
  group_by(FYEAR, Form) %>%
  summarise(count = n()) %>%
  spread(FYEAR, count)

  ATO_Tot[is.na(ATO_Tot)]<- 0
  
names(ATO_Tot)[names(ATO_Tot)=="Form"] <- "Type"

pander(ATO_Tot, caption = "Annual Frequency of ATOs Issued for Minors, by Order Type, FY10-FY15")

ATO_Provider<- ATO %>%
  group_by(FYEAR, ATO.Category) %>%
  summarise(count = n()) %>%
  spread(FYEAR, count)

names(ATO_Provider)[names(ATO_Provider)=="ATO.Category"] <- "Transportation Provider"
names(ATO_Provider)[names(ATO_Provider)=="2010"] <- "FY10"
names(ATO_Provider)[names(ATO_Provider)=="2011"] <- "FY11"
names(ATO_Provider)[names(ATO_Provider)=="2012"] <- "FY12"
names(ATO_Provider)[names(ATO_Provider)=="2013"] <- "FY13"
names(ATO_Provider)[names(ATO_Provider)=="2014"] <- "FY14"
names(ATO_Provider)[names(ATO_Provider)=="2015"] <- "FY15"

ATO_Provider[is.na(ATO_Provider)]<- 0

pander(ATO_Provider, caption = "Annual Frequency of ATOs Issued for Minors, by Transportation Provider, FY10-FY15")
