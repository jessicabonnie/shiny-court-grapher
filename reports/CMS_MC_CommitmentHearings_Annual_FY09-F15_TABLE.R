##################
# Draws annual table of Adult Mental Commitment Hearings from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_Annual_Initial_Hearings <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="TRUE", FYear>2009) %>%
  group_by(FYear) %>%
  summarise(count = n())

names(CMS_Annual_Initial_Hearings)[names(CMS_Annual_Initial_Hearings)=="FYear"] <- "Fiscal Year"
names(CMS_Annual_Initial_Hearings)[names(CMS_Annual_Initial_Hearings)=="count"] <- "# Commitment Hearings"

#pander(CMS_Annual_Initial_Hearings, caption  = "Annual Frequency of commitment Hearings Involving Adults, FY10-FY15", split.table = Inf)