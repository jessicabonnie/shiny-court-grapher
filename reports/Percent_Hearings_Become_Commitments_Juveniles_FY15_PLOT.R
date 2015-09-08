
library(dplyr)
library(stringr)
library(tidyr)
library(pander)
library(ggplot2)

rcrds <- read.csv("data/PostSilence_8_26.txt")


JCMS <- filter(rcrds, YEAR_OF_FILING==2015, CASE_TYPE_CD=="MC")

JCMSMonthly1 <- 
  JCMS %>%
  filter(CASE_TYPE_CD=="MC") %>%
  group_by(MONTH_OF_FILING) %>%
  summarise(count = n())

JCMSMonthly2 <- 
  JCMS %>%
  filter(CASE_TYPE_CD=="MC", FINAL_DISP_CD=="I") %>%
  group_by(MONTH_OF_FILING) %>%
  summarise(count2 = n())

names(JCMSMonthly)[names(JCMSMonthly)=="MONTH_OF_FILING"] <- "Month"


JCMS3 <- merge(JCMSMonthly1, JCMSMonthly2, by = c("MONTH_OF_FILING"), all.x = TRUE)

JCMS4<- gather(JCMS3, Type, number, count, count2)



p <-  ggplot(JCMS4, aes(x=MONTH_OF_FILING, y=number, fill=Type)) + geom_area(stat="identity", position="identity") + geom_line(aes(ymax=number)) + geom_text(size=3, aes(label=number, hjust=0.5, vjust=2)) + xlab("Month") + ylab("Count") +  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun")) +  scale_fill_discrete(labels=c("Hearings", "Orders"))


p + theme(plot.title = element_text(size=10))+ ggtitle("Monthly Frequencies of Commitment Hearings and Involuntary Commitment Orders, 2015")
