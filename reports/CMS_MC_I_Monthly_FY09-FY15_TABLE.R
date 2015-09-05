##################
# Draws monthly table of Adult Mental Commitment Orders from CMS data
##################
#source("reports/CMS_prep.R")

library(dplyr)
library(pander)
library(tidyr)

CMS_MonthlyCommitmentsInitial <- 
  CMS %>%
  filter(CASE.TYP=="MC", HEAR.RSLT=="I", Initial=="TRUE") %>%
  group_by(Initial, FYMonthAbbrev, FYear) %>%
  summarise(count = n()) %>%
  spread(FYear, count) %>%
  select(-Initial)

names(CMS_MonthlyCommitmentsInitial)[names(CMS_MonthlyCommitmentsInitial)=="FYMonthAbbrev"] <- "Month"

#pander(CMS_MonthlyCommitmentsInitial, caption  = "Monthly Frequency of Commitment Orders for Adults (Initial Only), FY09-FY15", split.table = Inf)
