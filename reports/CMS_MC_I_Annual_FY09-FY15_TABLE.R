##################
# Draws annual table of Adult Mental Commitment Orders from CMS data
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

names(CMS_AnnualCommitmentsInitial)[names(CMS_AnnualCommitmentsInitial)=="FYear"] <- "Fiscal Year"
names(CMS_AnnualCommitmentsInitial)[names(CMS_AnnualCommitmentsInitial)=="count"] <- "# Involuntary Commitment Orders"

# pander(CMS_AnnualCommitmentsInitial, caption  = "Annual Frequency of Commitment Orders for Adults (Initial Only), FY10-FY15", split.table = Inf)
