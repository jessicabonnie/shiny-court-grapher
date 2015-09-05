##################
# Draws annual table of Adult Mental Commitment Orders from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_OT_JAT_Annual <- 
  CMS %>%
  filter(CASE.TYP=="OT", HEAR.RSLT=="JG", Initial=="TRUE", FYear>2009) %>%
  group_by(FYear) %>%
  summarise(count = n())


CMS_OT_JAT_Annual_Plot <-  ggplot(CMS_OT_JAT_Annual, aes(x=FYear, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2)) + ylab("Number of Involuntary Commitments Orders") +  xlab("Fiscal Year")
#CMS_OT_JAT_Annual_Plot + ylim(0,2000) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Annual Frequency of Judicial Authorizations of Treatment Granted for Adults, FY010-FY15") + theme(plot.title = element_text(size=12))


