library(stringr)
library(lubridate
        )
CMS <- read.csv("../data/CMS_8_26.txt")

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

#Force FIPs into the correct format
CMS$FIPS <- str_pad(as.character(CMS$Fips.Code),3,side="left",pad="0")
