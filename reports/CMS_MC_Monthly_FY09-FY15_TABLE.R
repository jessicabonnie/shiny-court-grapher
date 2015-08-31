##################
# Draws monthly table of Adult Mental Commitment Hearings from CMS data
# Fiscal Months on the rows and Fiscal Years in the columns
##################
source("reports/CMS_prep.R")


# load all necessary packages
library(dplyr)
library(tidyr)
library(pander)

#need to also group by recommitment vs. initial.
CMS_MonthlyHearings <- 
  CMS %>%
  filter(CASE.TYP=="MC") %>%
  group_by(FYMonthAbbrev, FYear) %>%
  summarise(count = n()) %>%
  spread(FYear, count)

pander(CMS_MonthlyHearings, caption  = "Monthly Frequency of Commitment Hearings for Adults, FY09-FY15")
