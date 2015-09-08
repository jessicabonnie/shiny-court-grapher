##################
# Prepares eMagistrate table for use.
# Adds columns for :  FIPS codes, Fiscal Quarters, Fiscal Years
##################


library(stringr)

emags <- read.csv("../data/ECOTDOFips08_14.txt")

#Path for running script outside of markdown
#emags <- read.csv("data/ECOTDOFips08_14.txt")

# Create Fiscal Year Variable
emags$FYear<- emags$Year
emags[emags$Month > 6, ]$FYear <- as.numeric(emags[emags$Month > 6, ]$Year) + 1

#Create Fiscal Quarter Variable
emags$FQtr <- emags$Month
emags[emags$Month == 7|emags$Month==8| emags$Month==9, ]$FQtr <- 1
emags[emags$Month == 10|emags$Month==11| emags$Month==12, ]$FQtr <- 2
emags[emags$Month == 1|emags$Month==2| emags$Month==3, ]$FQtr <- 3
emags[emags$Month == 4|emags$Month==5| emags$Month==6, ]$FQtr <- 4

# Create abbreviated month column, factored in accordance with fiscal calendar
emags$FYMonthAbbrev <- factor(substr(month.name[emags$Month],1,3),levels=substr(c(month.name[7:12],month.name[1:6]),1,3))

# Create a uniq identifier for the month (may or may not be needed)
emags$month_id <- factor(paste(emags$FYear, str_pad(as.character(emags$Month), 2, side="left", pad="0"), sep="-"))

#Force FIPs into the correct format
emags$FIPS <- str_pad(as.character(emags$Fips.Code),3,side="left",pad="0")

#Remove unneccessary columns
emags <- emags[,!names(emags) %in% c("Fips.Code")]

