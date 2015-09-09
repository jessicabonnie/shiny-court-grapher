
library(dplyr)
library(tidyr)

source("reports/CMS_prep.R")
CSB <- read.csv("resources/CSB_FIPS_GIS.csv")
CSB$FIPS <- paste0(str_pad(as.character(CSB$FIPS), 3, side="left", pad="0"),"G")


thing <- merge(CMS,CSB, by=c("FIPS"))
thing <- select(thing,FYear,CSBName,OBJECTID,FID,HEAR.RSLT, CASE.TYP)

CMS_Invol <- 
  filter(thing, CASE.TYP =="MC", FYear > 2009) %>%
  group_by(CSBName,FYear, HEAR.RSLT)%>%
  tally %>%
  group_by(CSBName,FYear) %>%
  mutate(pct=(100*n)/sum(n)) %>%
  select(-n)%>%
  spread(HEAR.RSLT,pct,fill = 0)
  

#In future this is all we need plus gis stuff??

CMS_Invol <- 
  filter(CMS, CASE.TYP =="MC", FYear > 2009) %>%
  group_by(Locality,FYear, HEAR.RSLT)%>%
  tally %>%
  group_by(Locality,FYear) %>%
  mutate(pct=(100*n)/sum(n)) %>%
  select(-n)%>%
  spread(HEAR.RSLT,pct,fill = 0)


