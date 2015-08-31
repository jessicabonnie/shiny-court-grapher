
source("reports/CMS_prep.R")
#CMS <- read.csv("data/CMS_8_26.txt")

# load all necessary packages
library(dplyr)
library(tidyr)
library(pander)

library(ggthemes)

library(stringr)
library(lubridate)


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

pander(CMS_MonthlyHearings, caption = "Monthly Frequency of Commitment Hearings for Adults, FY10-FY15")
p <- 
  ggplot(CMS_MonthlyHearings2, aes(factor(FYMonthAbbrev), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of Commitment Hearings") +
  xlab("Month")

p + ylim(1200, max(CMS_MonthlyHearings2$count)) + geom_line(size=1.2) + geom_point(aes(shape=factor(FYear)), size=3) + scale_colour_discrete(name  ="Year") + scale_shape_discrete(name="Year") + ggtitle("Monthly Frequency Commitment Hearings for Adults, FY2010-FY2015")
