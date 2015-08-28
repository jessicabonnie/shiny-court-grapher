CMS <- read.csv("data/CMSPLUS_8_27.txt")

# load all necessary packages
library(dplyr)
library(ggthemes)
library(pander)
library(stringr)

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
CMS$FIPS <- str_pad(as.character(CMS$FIPS),3,side="left",pad="0")


#NOTE: ATTEMPT TO MAKE VARIABLE FOR RECOMMITMENT VS INITIAL HEARINGS. paycode 41 & 46 = recommitment. what is 21?
CMS$initial[CMS$PAY.CD=="41"|CMS$PAY.CD=="46"] <- "0"
CMS$initial[CMS$PAY.CD=="21"] <- "1"
CMS$initial[is.na(CMS$PAY.CD)]<- "1"

#creating a chart of monthly hearing counts by FY. Note, for this dataset I need to exclude july and august of FY09
#need to also group by recommitment vs. initial.
CMS_MonthlyHearings <- 
  CMS %>%
  filter(CASE.TYP=="MC") %>%
  group_by(FYMonthAbbrev, FYear) %>%
  summarise(count = n()) %>%
  spread(FYear, count)
 
pander(CMS_MonthlyHearings, caption = "Monthly Frequency of Commitment Hearings for Adults, FY10-FY15")

CMS_MonthlyHearings2 <- 
  CMS %>%
  filter(CASE.TYP=="MC") %>%
  group_by(FYMonthAbbrev, FYear) %>%
  summarise(count = n())%>%
  filter(FYear!="2009")

p <- 
  ggplot(CMS_MonthlyHearings2, aes(factor(FYMonthAbbrev), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of Commitment Hearings") +
  xlab("Month")

p + ylim(1200, max(CMS_MonthlyHearings2$count)) + geom_line(size=1.2) + geom_point(aes(shape=factor(FYear)), size=3) + scale_colour_discrete(name  ="Year") + scale_shape_discrete(name="Year") + ggtitle("Monthly Frequency Commitment Hearings for Adults, FY2010-FY2015")
