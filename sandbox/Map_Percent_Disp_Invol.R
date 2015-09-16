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


thing <- merge(CMS,CSB, by=c("FIPS"))
thing <- select(thing,FYear,CSBName,OBJECTID,FID,HEAR.RSLT, CASE.TYP)

CMS_Invol <- 
  filter(thing, CASE.TYP =="MC", FYear > 2009) %>%
  group_by(CSBName, FID, FYear, HEAR.RSLT)%>%
  tally %>%
  group_by(CSBName, FID, FYear) %>%
  mutate(pct=(100*n)/sum(n)) %>%
  select(-n)%>%
  spread(HEAR.RSLT,pct,fill = 0)




# read in shp files
csb <- readOGR(dsn = "map/.", layer = "CSBfinal_1")

# create id variable
csb@data$id <- rownames(csb@data)

whynot <- csb
whynot@data <- merge(whynot@data,CMS_InvolFY15,
                    all.x=T,by.x=c("id"),by.y=c("FID"))

whynot@data <- select(whynot@data, -CSB_Name, -Percent_Ve, -Number_Vet, -NumberEval, -HPR)

CMS_Invol$FID<- as.character(CMS_Invol$FID)
CMS_InvolFY15<- filter(CMS_Invol, FYear==2015)


# create dataframe so ggplot can do its thing
csb.points <- fortify(whynot, region="id")

csb.pointsplus<- merge(csb.points, CMS_InvolFY15, by.x=c("id"), by.y=c("FID"))

csb.disp.df <- right_join(csb.pointsplus, csb@data, by="id")


csb.dispTest.df <- select(csb.disp.df, -CSB_Name, -Percent_Ve, -Number_Vet, -NumberEval, -HPR)

csb.disp.df2 <- merge(csb.disp.df, CMS_InvolFY15, by.x=c("id"), by.y=c("FID"), all.x=TRUE)


csb.dispTest2.df <- right_join(csb.points, whynot@data, by="id")


# ggplot code for plotting
ggplot(csb.dispTest2.df, aes(x=long,y=lat,group=group,fill=I,fillcol="red")) + 
        geom_polygon() +
        geom_path(color="white")

