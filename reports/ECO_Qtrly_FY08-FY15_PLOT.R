##################
# Draws quarterly graph of adult ECOs from eMagistrate data
# One line, with fiscal quarters on the x axis and counts on the y axis
##################
# source("reports/emagistrate_prep.R")
# library(dplyr)
# library(pander)
# library(ggplot2)

ECO <- 
  emags %>%
  filter(Type=="ECO", FYear>2007)


ECOquarterly <- 
  group_by(ECO, FQtr, FYear) %>%
  summarise(count = sum(Process.Count))


ECOquarterly$FQtr <- factor(ECOquarterly$FQtr)


ECOQuarterlyPlot <- 
  ggplot(ECOquarterly, aes(factor(FQtr), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of ECOs") +
  xlab("Fiscal Quarter")

#ECOQuarterlyPlot + ylim(1200,max(ECOquarterly$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Fiscal Year")+ geom_point(aes(shape=factor(FYear)), size=3) + scale_shape_discrete(name="Fiscal Year")+ ggtitle("Quarterly Frequency of ECOs Issued for Adults, FY2008-FY2015")
