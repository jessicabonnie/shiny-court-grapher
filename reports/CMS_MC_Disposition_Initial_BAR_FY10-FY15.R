# Draws bar graph of annual Adult Commitment Hearing Dispositions from CMS data
##################
#source("reports/CMS_prep.R")

#library(dplyr)
#library(pander)
#library(tidyr)

CMS_AnnualMCDispositionInitial <- 
  CMS %>%
  filter(CASE.TYP=="MC", Initial=="TRUE", HEAR.RSLT== "MO"| HEAR.RSLT=="V"|HEAR.RSLT=="I"|HEAR.RSLT=="D", FYear>2009) %>%
  group_by(FYear, HEAR.RSLT) %>%
  summarise(count = n())

names(CMS_AnnualMCDispositionInitial)[names(CMS_AnnualMCDispositionInitial)=="D"] <- "Dismissal"

CMS_AnnualMCDispositionInitial$HEAR.RSLT <- factor(CMS_AnnualMCDispositionInitial$HEAR.RSLT,
                                        labels = c("Dismissal", "Involuntary", "MOT", "Voluntary"))


CMS_AnnualMCDispositionInitial_Plot<- ggplot(data=CMS_AnnualMCDispositionInitial, aes(x=factor(FYear), y=count, fill=HEAR.RSLT)) +
  geom_bar(stat="identity", position=position_dodge())

#CMS_AnnualMCDispositionInitial_Plot + xlab("Fiscal Year") + ylab("Counts of Dispositions at Initial Hearings") +
  #ggtitle("Frequencies of Dispositions at Initial Commitment Hearings Involving Adults, FY10-FY15") + theme(plot.title = element_text(size=12)) + scale_fill_hue(name="Disposition")

