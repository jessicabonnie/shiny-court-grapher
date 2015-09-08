library(stringr)
library(lubridate)
library(pander)
library(dplyr)
library(tidyr)
library(ggplot2)

 #source ("reports/CMS_prep.R")

#CMS <- read.csv("data/CMS_8_26.txt")
#FIPS_Codes <- read.csv("/data/FIPS_R.csv")

########### HERE'S THE MEAT #########


CMS_MOT_TOPFIPS <- filter(CMS, CASE.TYP =="MC", HEAR.RSLT %in% c("MO", "I")) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & !Initial), "TYPE4", NA)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & Initial), "TYPE3", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & Initial), "TYPE1", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & !Initial), "TYPE2", MOT_TYPE)) %>%
  mutate(MOT_TYPE=factor(MOT_TYPE)) %>%
  group_by(Locality,MOT_TYPE, FYear)%>%
  summarise(count = n())

CMS_MOT_TOPFIPS <- CMS_MOT_TOPFIPS[complete.cases(CMS_MOT_TOPFIPS),]

CMS_MOT_TOPFIPS <- spread(CMS_MOT_TOPFIPS,MOT_TYPE,count)

CMS_MOT_TOPFIPS[is.na(CMS_MOT_TOPFIPS)] <- 0

CMS_MOT_TOPFIPS$Total <- CMS_MOT_TOPFIPS$TYPE1 + CMS_MOT_TOPFIPS$TYPE2 + CMS_MOT_TOPFIPS$TYPE3 + CMS_MOT_TOPFIPS$TYPE4

names(CMS_MOT_TOPFIPS) <- c("Locality", "FYear","Direct", "New\nHearing", "Discharge\nInitial", "Discharge\nRecommitment", "Total")
CMS_MOT_TOPFIPS$Locality <- gsub("/","/\n",CMS_MOT_TOPFIPS$Locality)
CMS_MOT_TOPFIPS$Locality <- gsub(" \\(","\n\\(",CMS_MOT_TOPFIPS$Locality)

CMS_MOT_TOPFIPS <- filter(CMS_MOT_TOPFIPS, Locality %in% c("Fairfax County","Nottoway","Prince William","Staunton"))


MOT_TopFIPS_Plot <-  ggplot(CMS_MOT_TOPFIPS, aes(x=FYear, y=Total,group=factor(Locality),color=factor(Locality))) + geom_line() + geom_point() + geom_text(size=3, aes(label=Total, hjust=0.5, vjust=2)) + ylab("Number of Orders for MOT (All Types)") +  xlab("Fiscal Year")
MOT_TopFIPS_Plot + ylim(0,75) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Annual Frequency of MOT Orders (All Types) in Top FIPS, FY09-FY15")
