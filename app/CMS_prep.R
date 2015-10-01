##################
# Prepares Adult CMS table for use. Requires both CMS table and a FIPS Code table.
# Adds columns for :  FIPS codes, Locality Names, Hearing Months, Hearing Years, Fiscal Years
#                     Fiscal Quarters, Month Abbreviations (factor sorted by fiscal order),
#                     Initial/Recommitment Boolean
##################


library(stringr)
library(lubridate)
library(dplyr)


CMS <- read.csv("../data/CMS_8_26.txt")
FIPS_Codes <- read.csv("../data/FIPS_R.csv")

#Paths for running script outside of markdown
#CMS <- read.csv("data/CMS_8_26.txt")
#FIPS_Codes <- read.csv("data/FIPS_R.csv")

#remove extraneous columns
CMS<- select(CMS, -ErrorType, -Notes, -CorrectionDate, -Received, -FilingDate, -ID)

# Strip Whitespace from some values
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

#Create FIPs column
CMS$FIPS <- substr(CMS$CASE.NUMBER, 1, 4)

#Include FIPS names
CMS <- merge(CMS, FIPS_Codes, by = c("FIPS"), all.x = TRUE)
CMS <- CMS[,!names(CMS) %in% c("SHORT_FIPS","COURT")]
names(CMS)[names(CMS)=="NAME"] <- "Locality"

#Use Pay Code to determine if Hearing is Initial
CMS$Initial <- ifelse (CMS$PAY.CD == 41 | CMS$PAY.CD == 46, FALSE, TRUE)
CMS$Initial [is.na(CMS$PAY.CD)] <- TRUE

# The first quarter of FY2009 (July, August, September) is incomplete, so it is ALWAYS removed from the dataset

CMS <- filter(CMS, !(FQtr == 1 & FYear==2009))