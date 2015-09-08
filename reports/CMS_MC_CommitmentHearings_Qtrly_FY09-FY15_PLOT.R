##################
# Draws quarterly graph of Adult Mental Initial Commitment Hearings from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)


CMS_QtrlyCommitmentHearingsInitial <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="TRUE") %>%
  group_by(FYear, FQtr) %>%
  summarise(count = n())


CMS_QtrlyCommitmentHearingsInitial$FQtr <- factor(CMS_QtrlyCommitmentHearingsInitial$FQtr)


CMS_QtrlyCommitmentHearingsInitialPlot <- 
  ggplot(CMS_QtrlyCommitmentHearingsInitial, aes(factor(FQtr), count, group=factor(FYear), color=factor(FYear))) + 
  geom_line() +
  ylab("Number of Initial Commitment Hearings") +
  xlab("Fiscal Quarter")

#CMS_QtrlyCommitmentHearingsInitialPlot + ylim(2500,max(CMS_QtrlyCommitmentHearingsInitial$count)) + geom_line(size=1.2) + scale_colour_discrete(name  ="Fiscal Year")+ geom_point(aes(shape=factor(FYear)), size=3) + scale_shape_discrete(name="Fiscal Year")+ ggtitle("Quarterly Frequency of Initial Commitment Hearings Involving Adults, FY2009-FY2015") +  theme(plot.title = element_text(size=12))
