library(stringr)
library(lubridate)
library(pander)
library(dplyr)
library(tidyr)

 source ("reports/CMS_prep.R")

#CMS <- read.csv("data/CMS_8_26.txt")
#FIPS_Codes <- read.csv("/data/FIPS_R.csv")

########### HERE'S THE MEAT #########
# Create table of the counts of 4 Different MOT Types for all localities
CMS_MOT_Annual <- filter(CMS, CASE.TYP =="MC", HEAR.RSLT %in% c("MO", "I"))%>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & !initial), "TYPE4", NA)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & initial), "TYPE3", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & initial), "TYPE1", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & !initial), "TYPE2", MOT_TYPE)) %>%
  mutate(MOT_TYPE=factor(MOT_TYPE)) %>%
  #   group_by(Locality,HEAR.RSLT, MOT, initial)%>%
  group_by(FYear, MOT_TYPE)%>%
  #   filter((HEAR.RSLT == "I" & (MOT == "Y") ) | HEAR.RSLT == "MO" ) %>%
  summarise(count = n()) 

CMS_MOT_Annual <- CMS_MOT_Annual[complete.cases(CMS_MOT_Annual),]

CMS_MOT_Annual <- spread(CMS_MOT_Annual, MOT_TYPE, count, fill=NA)

CMS_MOT_Annual[is.na(CMS_MOT_Annual)] <- 0

CMS_MOT_Annual$Total <- CMS_MOT_Annual$TYPE1 + CMS_MOT_Annual$TYPE2 + CMS_MOT_Annual$TYPE3 + CMS_MOT_Annual$TYPE4

names(CMS_MOT_Annual) <- c("Fiscal Year", "Direct", "New\nHearing", "Discharge\nInitial", "Discharge\nRecommitment", "Total")


#pander(CMS_MOT_Annual, caption= "Fiscal Year MOT Counts by Type", keep.line.breaks = TRUE,split.table = Inf))
