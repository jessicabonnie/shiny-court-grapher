
##################
# Draws quarterly trends graph of Adult Mental Commitment Orders from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_QtrlyCommitmentsInitial <- 
  CMS %>%
  filter(CASE.TYP=="MC", HEAR.RSLT=="I", Initial=="TRUE") %>%
  group_by(FQtr, FYear) %>%
  summarise(count = n())


CMS_QtrlyCommitmentsInitial$FQtr <- factor(CMS_QtrlyCommitmentsInitial$FQtr)


CMS_Qtrly_Long<-
  unite(CMS_QtrlyCommitmentsInitial, Fyear_FQtr, FYear, FQtr, sep="-")

CMS_Qtrly_Long_Plot <- 
  ggplot(CMS_Qtrly_Long, aes(x=Fyear_FQtr, y=count, group=1)) + geom_line() +
  ylab("Number of Involuntary Commitment Orders") +   xlab("Fiscal Quarter")


#CMS_Qtrly_Long_Plot + ylim(2400,max(CMS_Qtrly_Long$count)) + geom_line(size=1.2) + ggtitle("Quarterly Involuntary Commitment Order Trends (Adults Only), FY2009-FY2015") + scale_x_discrete(labels=c("09-2", "09-3", "09-4", "10-1", "10-2", "10-3", "10-4", "11-1", "11-2", "11-3", "11-4", "12-1", "12-2", "12-3", "12-4", "13-1", "13-2", "13-3", "13-4", "14-1", "14-2", "14-3", "14-4", "15-1", "15-2","15-3", "15-4")) + theme(axis.text.x = element_text(angle=90))
