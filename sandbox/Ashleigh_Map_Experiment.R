# read in case management system data
source("reports/CMS_prep.R")

# load all packages
library(rgdal)
library(mapproj)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggmap)

# fix for gpc error
library(gpclib)
gpclibPermit()

# read in shp files
csb <- readOGR(dsn = "map/.", layer = "CSBfinal_1")

# create id variable
csb@data$id <- rownames(csb@data)
csb@data$cases <- 

# create dataframe so ggplot can do its thing
csb.points <- fortify(csb, region="id")
csb.df <- right_join(csb.points, csb@data, by="id")

addresses<-read.csv("Juvenile_Facility_Addresses.csv")
colnames(addresses) <- c("Name", "Address", "City", "State", "Zip")
head(addresses)

map("state", "VIRGINIA")

geocode('1355 Richmond Road, Staunton, VA', output="latlon", source="google")
geocode('1200 Sam Perry Boulevard,	Fredericksburg,	VA', output="latlon", source="google")
geocode('1900 Tate Springs Road,	Lynchburg,	VA', output="latlon", source="google")
geocode('2960 Sleepy Hollow Road,	Falls Church,	VA', output="latlon", source="google")
geocode('2017 South Jefferson Street,	Roanoke,	VA', output="latlon", source="google")
geocode('1902 Braeburn Drive,	Salem, VA', output="latlon", source="google")
geocode('350 Polar Drive,	Petersburg,	VA', output="latlon", source="google")
geocode('7101 Jahnke Road,	Richmond,	VA', output="latlon", source="google")
geocode('515 North 10th Street,	Richmond,	VA', output="latlon", source="google")
geocode('301 Fort Lane,	Portsmouth, VA', output="latlon", source="google")
geocode('860 Kempsville Road,	Norfolk,	VA', output="latlon", source="google")
geocode('3636 High Street,	Portsmouth,	VA', output="latlon", source="google")
geocode('2244 Executive Drive,	Hampton,	VA', output="latlon", source="google")



# ggplot code for plotting
ggplot(csb.df) + 
        aes(long,lat,group=group) + 
        geom_polygon(color="white", fill="light blue") +
        geom_path(color="white") + geom_point(x=-76.39, y=37.05, color="red") + geom_point(x=-76.35, y=36.84, color="red") + geom_point(x=-76.19, y=36.85, color="red") + geom_point(x=-76.307, y=36.84, color="red") + geom_point(x=-76.7, y=37.3, color="red")
