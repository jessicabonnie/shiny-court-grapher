#ATO <- read.csv("data/ATO with TimeInfo_8_26_15.txt")

ATO<-filter(ATO, Form=="ECOJ"| Form=="TDOJ")

ATO_Tot<- ATO %>%
  group_by(QTR) %>%
  summarise(count = n())

ATO_Tot$QTR <- factor(ATO_Tot$QTR)

ATO_Annual_Plot <-  ggplot(ATO_Tot, aes(x=QTR, y=count, group=1)) + geom_line() +
  ylab("Number of ATOs") +   xlab("Fiscal Quarter")

#ATO_Annual_Plot + ylim(0,60) + geom_line(size=1.2) + ggtitle("Annual Frequency of Alternative Transportation Orders Issued for Minors, FY10-FY15") + scale_x_discrete(labels=c("10-1", "10-2", "10-3", "10-4", "11-1", "11-2", "11-3", "11-4", "12-1", "12-2", "12-3", "12-4", "13-1", "13-2", "13-3", "13-4", "14-1", "14-2", "14-3", "14-4", "15-1", "15-2","15-3", "15-4")) + theme(axis.text.x = element_text(angle=90))
