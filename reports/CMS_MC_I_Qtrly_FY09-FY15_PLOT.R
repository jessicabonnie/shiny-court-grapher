##################
# Draws quarterly graph of Adult Mental Commitment Orders from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_QtrlyCommitmentsInitial <- 
  CMS %>%
  filter(CASE.TYP=="MC", HEAR.RSLT=="I", Initial=="TRUE") %>%
  group_by(FYear, FQtr) %>%
  summarise(count = n())


CMS_QtrlyCommitmentsInitial$FQtr <- factor(CMS_QtrlyCommitmentsInitial$FQtr)


CMS_QtrlyCommitmentsInitialPlot <- 
  ggplot(CMS_QtrlyCommitmentsInitial, aes(factor(FQtr), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of Inbluntary Commitment Orders") +
  xlab("Fiscal Quarter")

#CMS_QtrlyCommitmentsInitialPlot + ylim(2500,max(CMS_QtrlyCommitmentsInitial$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Fiscal Year")+ geom_point(aes(shape=factor(FYear)), size=3) + scale_shape_discrete(name="Fiscal Year")+ ggtitle("Quarterly Frequency of Commitment Orders Issued for Adults, FY2009-FY2015")
