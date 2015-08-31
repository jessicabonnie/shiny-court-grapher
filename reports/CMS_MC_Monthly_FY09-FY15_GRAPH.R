##################
# Draws monthly table of Adult Mental Commitment Hearings from CMS data
# One line per year, with fiscal months on the x axis and counts on the y axis
##################
source("reports/CMS_prep.R")


# load all necessary packages
library(dplyr)
library(tidyr)
library(pander)
library(ggthemes)

CMS_MonthlyHearingsPlot <- 
  CMS %>%
  filter(CASE.TYP=="MC") %>%
  group_by(FYMonthAbbrev, FYear)%>%
  summarise(count = n())

MCplot <- 
  ggplot(CMS_MonthlyHearingsPlot, aes(factor(FYMonthAbbrev), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of Commitment Hearings") +
  xlab("Month")

MCplot + ylim(1200, max(CMS_MonthlyHearingsPlot$count)) + geom_line(size=1.2) + geom_point(aes(shape=factor(FYear)), size=3) + scale_colour_discrete(name  ="Fiscal Year") + scale_shape_discrete(name="Fiscal Year") + 
  ggtitle("Monthly Frequency Commitment Hearings for Adults, FY2009-FY2015")
