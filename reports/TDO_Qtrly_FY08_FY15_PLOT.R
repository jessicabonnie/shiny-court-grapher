##################
# Draws quarterly graph of adult ECOs from eMagistrate data
# One line, with fiscal quarters on the x axis and counts on the y axis
##################
# source("reports/emagistrate_prep.R")
# library(dplyr)
# library(pander)
# library(ggplot2)

TDO <- 
  emags %>%
  filter(Type=="TDO", FYear>2007)


TDOquarterly <- 
  group_by(TDO, FQtr, FYear) %>%
  summarise(count = sum(Process.Count))


TDOquarterly$FQtr <- factor(TDOquarterly$FQtr)


TDOQuarterlyPlot <- 
  ggplot(TDOquarterly, aes(factor(FQtr), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of TDOs") +
  xlab("Fiscal Quarter")

#TDOQuarterlyPlot + ylim(1200,max(TDOquarterly$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Fiscal Year")+ geom_point(aes(shape=factor(FYear)), size=3) + scale_shape_discrete(name="Fiscal Year")+ ggtitle("Quarterly Frequency of TDOs Issued for Adults, FY2008-FY2015")
