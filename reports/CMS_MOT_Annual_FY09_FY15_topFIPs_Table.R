library(stringr)
library(lubridate)
library(pander)
library(dplyr)
library(tidyr)

 #source ("reports/CMS_prep.R")

#CMS <- read.csv("data/CMS_8_26.txt")
#FIPS_Codes <- read.csv("/data/FIPS_R.csv")

########### HERE'S THE MEAT #########


CMS_MOT <- filter(CMS, CASE.TYP =="MC", HEAR.RSLT %in% c("MO", "I")) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & !Initial), "TYPE4", NA)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & Initial), "TYPE3", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & Initial), "TYPE1", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & !Initial), "TYPE2", MOT_TYPE)) %>%
  mutate(MOT_TYPE=factor(MOT_TYPE)) %>%
  #   group_by(Locality,HEAR.RSLT, MOT, Initial)%>%
  group_by(Locality,MOT_TYPE, FYear)%>%
  #   filter((HEAR.RSLT == "I" & (MOT == "Y") ) | HEAR.RSLT == "MO" ) %>%
  summarise(count = n())

CMS_MOT <- CMS_MOT[complete.cases(CMS_MOT),]

CMS_MOT <- spread(CMS_MOT,MOT_TYPE,count)

CMS_MOT[is.na(CMS_MOT)] <- 0

CMS_MOT$Total <- CMS_MOT$TYPE1 + CMS_MOT$TYPE2 + CMS_MOT$TYPE3 + CMS_MOT$TYPE4

names(CMS_MOT) <- c("Locality", "FYear","Direct", "New\nHearing", "Discharge\nInitial", "Discharge\nRecommitment", "Total")
CMS_MOT$Locality <- gsub("/","/\n",CMS_MOT$Locality)
CMS_MOT$Locality <- gsub(" \\(","\n\\(",CMS_MOT$Locality)

CMS_MOT <- filter(CMS_MOT, Locality %in% c("Fairfax","Nottoway","Prince William","Staunton"))
