#gittest line
rcrds <- read.csv("data/JCMS.txt")

# load all necessary packages
library(dplyr)
library(ggthemes)
library(pander)

JCMS <- filter(rcrds, YEAR_OF_FILING==2015)


JCMSMonthly <- 
  JCMS %>%
  filter(CASE_TYPE_CD=="MC") %>%
  group_by(MONTH_OF_FILING) %>%
  summarise(count = n())

  
pander(JCMSMonthly, caption = "Monthly Frequency of Commitment Hearings for Minors, 2015")

