# load up area shape file
install.packages("maptools")
library("maptools")

area <- readShapePoly("C:\\Users\\Manas\\Desktop\\street_shapefile\\streets.shp")

gor = readShapeSpatial('C:\\Users\\Manas\\Desktop\\street_shapefile\\streets.shp')
plot(gor)

summary(gor)


install.packages("rgdal")
library("rgdal")
shape <- readOGR("C://Users//Manas//Desktop//street_shapefile//","streets")


install.packages("shapefiles")
library("shapefiles")

a <- read.shapefile("C:\\Users\\Manas\\Desktop\\street_shapefile\\streets")

install.packages("ggmap")
library("ggmap")

shapefile_df <- fortify(a)

#####################################################################
require("plyr")

syracuse <- readOGR(dsn=".", layer = "streets")
syracuse$data$id = rownames(syracuse$data)
syracuse.points = fortify(syracuse, region = "id")
syracuse.df <- join(syracuse.points, syracuse$data, by = "id")

syracuse$

getwd()

install.packages("PBSmapping")
library("PBSmapping")

myshapefile <- importShapefile("C:\\Users\\Manas\\Desktop\\street_shapefile\\streets.prj")


library(ggmap)
library(mapproj)

maps <- get_map(location = "syracuse", zoom = 13)
ggmap(maps)


potholes <- read.csv("potholes.csv")
View(potholes)

ggmap(maps) +
  geom_point(data = potholes, aes(x = Latitude, y = Longitude, col = "green"))