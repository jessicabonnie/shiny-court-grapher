
library(dplyr)
library(stringr)
library(tidyr)
library(pander)
library(ggplot2)

source("reports/emagistrate_prep.R")
rcrds <- read.csv("data/PostSilence_8_26.txt")

TDOJ<-filter(emags, Type=="TDOJ")

TDOJmonthly <- 
  group_by(TDOJ, FYear, Month) %>%
  summarise(count = sum(Process.Count))

TDOJJanJun15<- filter (TDOJmonthly, FYear==2015, Month < 7)




JCMS <- filter(rcrds, YEAR_OF_FILING==2015, CASE_TYPE_CD=="MC")

JCMSMonthly <- 
  JCMS %>%
  filter(CASE_TYPE_CD=="MC") %>%
  group_by(MONTH_OF_FILING) %>%
  summarise(count2 = n())

names(JCMSMonthly)[names(JCMSMonthly)=="MONTH_OF_FILING"] <- "Month"


emagsJCMS <- merge(JCMSMonthly, TDOJJanJun15, by = c("Month"), all.x = TRUE)

emagsJCMS<- gather(emagsJCMS, Type, number, count, count2)



p <-  ggplot(emagsJCMS, aes(x=Month, y=number, fill=Type)) + geom_area(stat="identity", position="identity") + geom_line(aes(ymax=number)) + geom_text(size=3, aes(label=number, hjust=0.5, vjust=2)) + xlab("Month") + ylab("Count") +  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun")) +  scale_fill_discrete(labels=c("Orders", "Hearings"))


p + theme(plot.title = element_text(size=10))+ ggtitle("Monthly Frequencies of TDOs and Commitment Hearings Involving Minors, 2015")
