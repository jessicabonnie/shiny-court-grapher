# helper script for the ilppp app

# load all packages
library(rgdal)
library(mapproj)
library(ggplot2)
library(dplyr)
library(maptools)
library(ggthemes)
library(gpclib)
library(ggmap)

# fix for gpc error
gpclibPermit()

# read in shp files
# note the file structure may change but you have to use the '.' syntax to get everything
csb <- readOGR(dsn = "../map/.", layer = "CSBfinal_1")

# create id variable
csb@data$id <- rownames(csb@data)

# create dataframe so ggplot can do its thing
csb.points <- fortify(csb, region="id")
csb.df <- right_join(csb.points, csb@data, by="id")

