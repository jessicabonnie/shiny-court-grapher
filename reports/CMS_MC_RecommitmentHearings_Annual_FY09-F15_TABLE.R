##################
# Draws annual table of Adult Mental Commitment Orders from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_Annual_Recommitment_Hearings <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="FALSE", FYear>2009) %>%
  group_by(FYear) %>%
  summarise(count = n())

names(CMS_Annual_Recommitment_Hearings)[names(CMS_Annual_Recommitment_Hearings)=="FYear"] <- "Fiscal Year"
names(CMS_Annual_Recommitment_Hearings)[names(CMS_Annual_Recommitment_Hearings)=="count"] <- "# Recommitment Orders"

pander(CMS_Annual_Recommitment_Hearings, caption  = "Annual Frequency of Recommitment Hearings for Adults, FY10-FY15", split.table = Inf)
