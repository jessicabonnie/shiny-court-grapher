# helper script for the ilppp app

# load all packages
library(rgdal)
library(mapproj)
library(ggplot2)
library(maptools)
library(ggthemes)
library(gpclib)
library(ggmap)
library(dplyr)
library(tidyr)
library(stringr)

# fix for gpc error
gpclibPermit()


# read in case management system data
#source("CMS_prep.R")

CSB <- read.csv("../resources/CSB_FIPS_GIS.csv")
CSB_FID <- read.csv("../resources/gis_joinfile.csv")

CSB_FID$FID <- as.numeric(CSB_FID$FID)

CSB$FIPS <- paste0(str_pad(as.character(CSB$FIPS), 3, side="left", pad="0"),"G")


CMS_Invol_pre <- merge(CMS,CSB, by=c("FIPS"))
CMS_Invol_pre <- select(CMS_Invol_pre,FYear,CSBName,OBJECTID,FID,HEAR.RSLT, CASE.TYP)

CMS_Invol_pre$FID <- as.numeric(CMS_Invol_pre$FID)

CMS_Invol <- 
  filter(CMS_Invol_pre, CASE.TYP =="MC", FYear > 2009) %>%
  group_by(CSBName, FID, FYear, HEAR.RSLT)%>%
  tally %>%
  group_by(CSBName, FID, FYear) %>%
  mutate(pct=(100*n)/sum(n)) %>%
  select(-n)%>%
  spread(HEAR.RSLT,pct,fill = 0)

csb_scaffold <- expand.grid(CSBName=unique(CMS_Invol$CSBName), FYear=unique(CMS_Invol$FYear)) 

csb_scaffold <- left_join(x=csb_scaffold, y= CSB_FID[ , c("CSBName", "FID")], by=c("CSBName"))
CMS_Invol<- left_join(x=csb_scaffold, y=CMS_Invol, by=c("CSBName", "FYear","FID"))

CMS_Invol[is.na(CMS_Invol)] <- 50

# read in shp files
# note the file structure may change but you have to use the '.' syntax to get everything
csb_intake <- readOGR(dsn = "../map/.", layer = "CSBfinal_1")
csb_intake <- spTransform(csb_intake, CRS("+proj=longlat +datum=WGS84"))
csb_intake@data$id <- as.numeric(rownames(csb_intake@data))






map_plot <- function (fy, dispos){
  csb <- csb_intake
  sub_cms_invol <- filter(CMS_Invol, FYear == fy)
  csb@data <- merge(csb@data,sub_cms_invol,
                    all.x=T,by.x=c("id"),by.y=c("FID"))
  csb@data <- select(csb@data, -CSB_Name, -Percent_Ve,
                     -Number_Vet, -NumberEval, -HPR)

# create dataframe so ggplot can do its thing
  csb.points <- fortify(csb, region="id")
  csb.points$id <- as.numeric(csb.points$id)

  plot_invol <- right_join(csb.points, csb@data, by="id")
#print(filter(plot_invol, CSBName == "Southside"))
print(dispos)
  ggplot(plot_invol, aes_string(x="long",y="lat",group="group",fill=dispos)) + 
    geom_polygon() + geom_path(color="white") 
}



# For testing


map_plot_test <- function (thing){
  csbtest <- csb_intake
  sub_cms_invol <- filter(CMS_Invol, FYear == 2014)
  csbtest@data <- merge(csbtest@data,sub_cms_invol,
                    all.x=T,by.x=c("id"),by.y=c("FID"))
  csbtest@data <- select(csbtest@data, -CSB_Name, -Percent_Ve, -Number_Vet, -NumberEval, -HPR)
  
  # create dataframe so ggplot can do its thing
  csbtest.points <- fortify(csbtest, region="id")
  csbtest.points$id <- as.numeric(csbtest.points$id)
  plot_invol <- right_join(csbtest.points, csbtest@data, by="id")
  #print(filter(plot_invol, CSBName == "Southside"))
  
  ggplot(plot_invol, aes(x=long,y=lat,group=group,fill=as.factor(id))) + 
    geom_polygon() + geom_path(color="white") + geom_polygon(data=filter(plot_invol, id == thing), color="black")
}