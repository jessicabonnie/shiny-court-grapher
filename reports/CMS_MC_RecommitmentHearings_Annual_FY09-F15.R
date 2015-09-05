##################
# Draws annual graph of Adult Recommitment Hearings from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_AnnualRecommitmentHearings <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="FALSE", FYear>2009) %>%
  group_by(FYear) %>%
  summarise(count = n())


CMS_Annual_Recommitments_Plot <-  ggplot(CMS_AnnualRecommitmentHearings, aes(x=FYear, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2)) + ylab("Number of Involuntary Commitments Orders") +  xlab("Fiscal Year")
#CMS_Annual_Recommitments_Plot + ylim(0,3000) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Annual Frequency of Involuntary Commitments Orders, FY010-FY15")
