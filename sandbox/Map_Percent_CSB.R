# read in case management system data
source("reports/CMS_prep.R")

# load all packages
library(rgdal)
library(mapproj)
library(ggplot2)
library(dplyr)
library(maptools)
library(ggthemes)
library(gpclib)

# fix for gpc error
gpclibPermit()


source("reports/CMS_prep.R")
CSB <- read.csv("resources/CSB_FIPS_GIS.csv")
CSB$FIPS <- paste0(str_pad(as.character(CSB$FIPS), 3, side="left", pad="0"),"G")


CMS_Invol_pre <- merge(CMS,CSB, by=c("FIPS"))
CMS_Invol_pre <- select(CMS_Invol_pre,FYear,CSBName,OBJECTID,FID,HEAR.RSLT, CASE.TYP)

CMS_Invol <- 
  filter(CMS_Invol_pre, CASE.TYP =="MC", FYear > 2009) %>%
  group_by(CSBName, FID, FYear, HEAR.RSLT)%>%
  tally %>%
  group_by(CSBName, FID, FYear) %>%
  mutate(pct=(100*n)/sum(n)) %>%
  select(-n)%>%
  spread(HEAR.RSLT,pct,fill = 0)

CMS_Invol$FID<- as.character(CMS_Invol$FID)
CMS_InvolFY15<- filter(CMS_Invol, FYear==2015)

# read in shp files
csb <- readOGR(dsn = "map/.", layer = "CSBfinal_1")

# create id variable
csb@data$id <- rownames(csb@data)
csb@data <- merge(csb@data,CMS_InvolFY15,
      all.x=T,by.x=c("id"),by.y=c("FID"))
csb@data <- select(csb@data, -CSB_Name, -Percent_Ve, -Number_Vet, -NumberEval, -HPR)

# create dataframe so ggplot can do its thing
csb.points <- fortify(csb, region="id")

plot_invol <- right_join(csb.points, csb@data, by="id")
csbnames <- na.omit(select(plot_invol,CSBName,long,lat,group)) %>%
  group_by(CSBName,group)%>%
  sample_n(1)

MFPLOT <- ggplot(plot_invol, aes(x=long,y=lat,group=group,fill=I)) + 
  geom_polygon() + geom_path(color="white")
MFPLOT
MFPLOT + annotate("text", x=csbnames$long,y=csbnames$lat,label=csbnames$CSBName)

