
source("reports/emagistrate_prep.R")
source("reports/CMS_prep.R")

library(dplyr)
library(stringr)
library(tidyr)
library(pander)
library(ggplot2)



TDO<-filter(emags, Type=="TDO", FYear> 2009)

TDO_Annual <- 
  group_by(TDO, FYear) %>%
  summarise(count = sum(Process.Count))

CMS <- filter (CMS, FYear> 2009)



CMS_Commit_Annual <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="TRUE") %>%
  group_by(FYear) %>%
  summarise(count2 = n())


CMS_Commit_Annual3 <- merge(TDO_Annual, CMS_Commit_Annual, by = c("FYear"), all.x = TRUE)

CMS_Commit_Annual4<- gather(CMS_Commit_Annual3, Type, number, count, count2)



p <-  ggplot(CMS_Commit_Annual4, aes(x=FYear, y=number, fill=Type)) + geom_area(stat="identity", position="identity") + geom_text(size=3, aes(label=number, hjust=0.5, vjust=2)) + xlab("Fiscal Year") + ylab("Count") +  scale_fill_discrete(labels=c("TDOs", "Hearings"))

p + ggtitle("Annual Frequencies of TDOs and Initial Commitment Hearings, FY10-FY15") + theme(plot.title = element_text(size=16))
