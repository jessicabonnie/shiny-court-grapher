##################
# Draws monthly table of Adult Mental Commitment Orders from CMS data
# One line per year, with fiscal months on the x axis and counts on the y axis
##################
#source("reports/CMS_prep.R")


# load all necessary packages
library(dplyr)
library(tidyr)
library(pander)
library(ggthemes)

CMS_MonthlyCommitmentsPlot <- 
  CMS %>%
  filter(CASE.TYP=="MC", HEAR.RSLT=="I", Initial==TRUE) %>%
  group_by(FYMonthAbbrev, FYear)%>%
  summarise(count = n())

MC_Iplot <- 
  ggplot(CMS_MonthlyCommitmentsPlot, aes(factor(FYMonthAbbrev), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of Involuntary Commitment Orders") +
  xlab("Month")

MC_Iplot + ylim(0, max(CMS_MonthlyCommitmentsPlot$count)) + geom_line(size=1.2) + geom_point(aes(shape=factor(FYear)), size=3) + scale_colour_discrete(name  ="Fiscal Year") + scale_shape_discrete(name="Fiscal Year") + 
  ggtitle("Monthly Frequency of Involuntary Commitment Orders for Adults (Initial Only), FY2009-FY2015")
