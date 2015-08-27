# read in case management system data
rcrds <- read.csv("data/PostSilence_8_26.txt")

# load all packages
library(rgdal)
library(mapproj)
library(ggplot2)
library(dplyr)
library(maptools)
library(ggthemes)

# these are confusingly bad names. oops

#all cases in 2015
JCMS <- filter(rcrds, YEAR_OF_FILING==2015)

#all MC cases ever
JCMS_MC_all <- filter(rcrds, CASE_TYPE_CD=="MC")

#all MC cases in 2015
JCMS_MC_15 <- filter(rcrds, YEAR_OF_FILING==2015, CASE_TYPE_CD=="MC")

# looking at frequencies of case types in 2015
JCMS_table_15<- count(JCMS, CASE_TYPE_CD)

#looking at frequencies of commitment hearing dispositions in 2015
JCMS_disposition <- count(JCMS_MC_15, FINAL_DISP_CD)


#looking at frequencies all of dispositions ever
JCMS_disposition_ever <- count(rcrds, FINAL_DISP_CD)

#looking at frequencies of all MC case dispositions ever
JCMS_MC_ever <- count(JCMS_MC_all, FINAL_DISP_CD)

#looking at frequencies of all dispositions in 2015
JCMS_disposition_all_15 <- count(JCMS, FINAL_DISP_CD)


#monthly counts of commmitment hearings

JCMSMonthly <- 
  JCMS %>%
  filter(CASE_TYPE_CD=="MC") %>%
  group_by(MONTH_OF_FILING) %>%
  summarise(count = n())

  
pander(JCMSMonthly, caption = "Monthly Frequency of Commitment Hearings for Minors, 2015")

p <-  ggplot(JCMSMonthly, aes(x=MONTH_OF_FILING, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2)) + ylab("Number of Commitment Orders") +  xlab("Month")

p + ylim(0,max(JCMSMonthly$count)) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Monthly Frequency of Commitment Hearings Involving Minors, January-June 2015")


#monthly counts of commitment orders
JCMSMonthly_I <- 
  JCMS %>%
  filter(CASE_TYPE_CD=="MC", FINAL_DISP_CD=="I") %>%
  group_by(MONTH_OF_FILING) %>%
  summarise(count = n())

pander(JCMSMonthly_I, caption = "Monthly Frequency of Commitment Orders for Minors, 2015")

p <-  ggplot(JCMSMonthly_I, aes(x=MONTH_OF_FILING, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2)) + ylab("Number of Commitment Orders") +  xlab("Month")

p + ylim(0,max(JCMSMonthly_I$count)) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Monthly Frequency of Commitment Orders Issued to Minors, January-June 2015")
