##################
# Draws quarterly graph of adult ECOs from eMagistrate data
# One line, with fiscal quarters on the x axis and counts on the y axis
##################
#source("reports/emagistrate_prep.R")
library(dplyr)
library(pander)
library(ggplot2)

ECO <- 
  emags %>%
  filter(Type=="ECO", FYear>2007)


ECO_Qtrly <- ECO %>%
  group_by(FQtr, FYear) %>%
  summarise(count = sum(Process.Count))


ECO_Qtrly$FQtr <- factor(ECO_Qtrly$FQtr)


ECO_Qtrly_Long<-
  unite(ECO_Qtrly, Fyear_FQtr, FYear, FQtr, sep="-")

ECO_Qtrly_Long_Plot <- 
  ggplot(ECO_Qtrly_Long, aes(x=Fyear_FQtr, y=count, group=1)) + geom_line() +
  ylab("Number of ECOs") +   xlab("Fiscal Quarter")


#ECO_Qtrly_Long_Plot + ylim(1000,max(ECO_Qtrly_Long$count)) + geom_line(size=1.2) + ggtitle("Quarterly ECO Trends (Adults Only), FY2008-FY2015") + scale_x_discrete(labels=c("08-1", "08-2", "08-3", "08-4", "09-1", "09-2", "09-3", "09-4", "10-1", "10-2", "10-3", "10-4", "11-1", "11-2", "11-3", "11-4", "12-1", "12-2", "12-3", "12-4", "13-1", "13-2", "13-3", "13-4", "14-1", "14-2", "14-3", "14-4", "15-1", "15-2","15-3", "15-4")) + theme(axis.text.x = element_text(angle=90))
