##################
# Draws annual graph of Adult Mental Commitment Orders from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_AnnualCommitmentsInitial <- 
  CMS %>%
  filter(CASE.TYP=="MC", HEAR.RSLT=="I", Initial=="TRUE", FYear>2009) %>%
  group_by(FYear) %>%
  summarise(count = n())


CMS_Annual_Commitments_Plot <-  ggplot(CMS_AnnualCommitmentsInitial, aes(x=FYear, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2)) + ylab("Number of Involuntary Commitments Orders") +  xlab("Fiscal Year")
#CMS_Annual_Commitments_Plot + ylim(10000,15000) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Annual Frequency of Involuntary Commitments Orders, FY010-FY15")
