library(stringr)
library(lubridate)
library(pander)
library(dplyr)
library(tidyr)

CMS <- read.csv("data/CMS_8_26.txt")
FIPS_Codes <- read.csv("data/FIPS_R.csv")

# Remove Extraneous columns from CMS table
CMS <- CMS[,!names(CMS) %in% c("Notes","Received","ErrorType","FilingDate","CorrectionDate")]

CMS$HEAR.RSLT <- str_trim(CMS$HEAR.RSLT)
CMS$CASE.TYP <- str_trim(CMS$CASE.TYP)
CMS$MOT <- str_trim(CMS$MOT)

#Create Month, Year
CMS$HEAR.DATE <- as.Date(CMS$HEAR.DATE, format = '%m/%d/%Y')
CMS$Year<- year(CMS$HEAR.DATE)
CMS$Month <- month(CMS$HEAR.DATE)

#Filter for no errors (keep or no?)
CMS<- filter(CMS, Error< 1|is.na(Error))

# Create Fiscal Year Variable
CMS$FYear<- CMS$Year
CMS[CMS$Month > 6, ]$FYear <- as.numeric(CMS[CMS$Month > 6, ]$Year) + 1

#Create Fiscal Quarter Variable
CMS$FQtr <- CMS$Month
CMS[CMS$Month == 7|CMS$Month==8| CMS$Month==9, ]$FQtr <- 1
CMS[CMS$Month == 10|CMS$Month==11| CMS$Month==12, ]$FQtr <- 2
CMS[CMS$Month == 1|CMS$Month==2| CMS$Month==3, ]$FQtr <- 3
CMS[CMS$Month == 4|CMS$Month==5| CMS$Month==6, ]$FQtr <- 4

# Create abbreviated month column, factored in accordance with fiscal calendar
CMS$FYMonthAbbrev <- factor(substr(month.name[CMS$Month],1,3),levels=substr(c(month.name[7:12],month.name[1:6]),1,3))

# Create a uniq identifier for the month (may or may not be needed)
CMS$month_id <- factor(paste(CMS$FYear, str_pad(as.character(CMS$Month), 2, side="left", pad="0"), sep="-"))

CMS$FIPS <- substr(CMS$CASE.NUMBER, 1, 4)
#Create FIPS names
CMS <- merge(CMS, FIPS_Codes, by = c("FIPS"), all.x = TRUE)
CMS <- CMS[,!names(CMS) %in% c("SHORT_FIPS","COURT")]
names(CMS)[names(CMS)=="NAME"] <- "Locality"

CMS$initial <- ifelse (CMS$PAY.CD == 41 | CMS$PAY.CD == 46, FALSE, TRUE)
CMS$initial [is.na(CMS$PAY.CD)] <- TRUE

CMS_MOT <- filter(CMS, CASE.TYP =="MC", HEAR.RSLT %in% c("MO", "I"))%>%
  group_by(Locality,HEAR.RSLT, MOT, initial)%>%
  filter((HEAR.RSLT == "I" & (MOT == "Y") ) | HEAR.RSLT == "MO" ) %>%
  summarise(count = n()) %>%
  spread(Locality,count)

CMS_MOT$MOT_TYPE <- "X"
CMS_MOT[CMS_MOT$HEAR.RSLT=="I" & CMS_MOT$MOT=="Y" & !CMS_MOT$initial ,]$MOT_TYPE <- "TYPE1"
CMS_MOT[CMS_MOT$HEAR.RSLT=="I" & CMS_MOT$MOT=="Y" & CMS_MOT$initial ,]$MOT_TYPE <- "TYPE2"
CMS_MOT[CMS_MOT$HEAR.RSLT=="MO"  & !CMS_MOT$initial ,]$MOT_TYPE <- "TYPE3"
CMS_MOT[CMS_MOT$HEAR.RSLT=="MO"  & CMS_MOT$initial ,]$MOT_TYPE <- "TYPE4"
#CMS_MOT <- as.data.frame(CMS_MOT)
CMS_MOT <- select(CMS_MOT,-initial,- MOT,- HEAR.RSLT)
CMS_MOT <- t(CMS_MOT)


# Trying to give MOT type column names
#CMS_MOT <- as.data.frame(CMS_MOT,colnames)
#colnames(CMS_MOT) <- CMS_MOT[49,]

CMS_MOT <- as.data.frame(CMS_MOT)

CMS_MOT<- rename(CMS_MOT, Stepdown_Recommitment_Discharge=V1, Stepdown_Initial_Discharge=V2, Stepdown_New_Hearing=V3, Direct=V4)
CMS_MOT <- CMS_MOT[-50,]
pander(CMS_MOT, caption= "MOT Types by Locality")


