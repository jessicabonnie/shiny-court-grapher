library(stringr)
library(lubridate)
library(pander)
library(dplyr)
library(tidyr)
library(ggplot2)

#source ("reports/CMS_prep.R")


########### HERE'S THE MEAT #########
# Create table of the counts of 4 Different MOT Types for all localities
CMS_MOT_Annual_Tot <- filter(CMS, CASE.TYP =="MC", HEAR.RSLT %in% c("MO", "I"))%>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & !Initial), "TYPE4", NA)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="I" & MOT=="Y" & Initial), "TYPE3", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & Initial), "TYPE1", MOT_TYPE)) %>%
  mutate(MOT_TYPE=ifelse((HEAR.RSLT=="MO"  & !Initial), "TYPE2", MOT_TYPE)) %>%
  filter(MOT_TYPE=="TYPE1"| MOT_TYPE=="TYPE2" | MOT_TYPE== "TYPE3" | MOT_TYPE=="TYPE4") %>%
  #   group_by(Locality,HEAR.RSLT, MOT, Initial)%>%
  group_by(FYear)%>%
  #   filter((HEAR.RSLT == "I" & (MOT == "Y") ) | HEAR.RSLT == "MO" ) %>%
  summarise(count = n()) 


MOT_Annual_Plot <-  ggplot(CMS_MOT_Annual_Tot, aes(x=FYear, y=count)) + geom_line() + geom_point() + geom_text(size=3, aes(label=count, hjust=0.5, vjust=2)) + ylab("Number of Orders for MOT (All Types)") +  xlab("Fiscal Year")

# MOT_Annual_Plot + ylim(0,350) + geom_line(size=1.2) + geom_point(size=3.5)+ ggtitle("Annual Frequency of MOT Orders (All Types), FY09-FY15")
