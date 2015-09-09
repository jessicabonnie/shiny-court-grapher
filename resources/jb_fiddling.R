
CSB <- read.csv("resources/CSB_FIPS.csv")
CSB$FIPS <- str_pad(as.character(CSB$FIPS), 3, side="left", pad="0")

FIPS_Codes$SHORT_FIPS <- str_pad(as.character(FIPS_Codes$SHORT_FIPS), 3, side="left", pad="0")

gisthing <- read.csv("resources/gis_joinfile.csv")

check <- merge(gisthing, CSB, by.x=c("CSBName"),by.y=c("CSB"),all=TRUE)

write.csv(check,file="resources/CSB_FIPS_GIS.csv",row.names=F)