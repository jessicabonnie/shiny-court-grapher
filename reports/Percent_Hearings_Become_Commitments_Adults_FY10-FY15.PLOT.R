source("reports/CMS_prep.R")

library(dplyr)
library(stringr)
library(tidyr)
library(pander)
library(ggplot2)

CMS <- filter (CMS, FYear> 2009)




CMS_Commit_Annual <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="TRUE") %>%
  group_by(FYear) %>%
  summarise(count = n())

CMS_Commit_Annual2 <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="TRUE", HEAR.RSLT=="I") %>%
  group_by(FYear) %>%
  summarise(count2 = n())


CMS_Commit_Annual3 <- merge(CMS_Commit_Annual, CMS_Commit_Annual2, by = c("FYear"), all.x = TRUE)

CMS_Commit_Annual4<- gather(CMS_Commit_Annual3, Type, number, count, count2)



p <-  ggplot(CMS_Commit_Annual4, aes(x=FYear, y=number, fill=Type)) + geom_area(stat="identity", position="identity") + geom_text(size=3, aes(label=number, hjust=0.5, vjust=2)) + xlab("Fiscal Year") + ylab("Count") +  scale_fill_discrete(labels=c("Hearings", "Commitments"))

p + ggtitle("Annual Frequencies of Initial Commitment Hearings and Involuntary Commitment Orders, FY10-FY15") + theme(plot.title = element_text(size=16))
