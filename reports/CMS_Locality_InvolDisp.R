
library(dplyr)
library(tidyr)

source("reports/CMS_prep.R")


CMS_Invol <- filter(CMS, CASE.TYP =="MC", HEAR.RSLT == "I")%>%
  group_by(Locality,HEAR.RSLT, Initial)%>%
  summarise(count = n()) %>%
  spread(Locality,count)




