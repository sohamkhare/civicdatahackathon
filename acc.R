install.packages("rgdal")
library(rgdal)
install.packages("ggmap")
library(ggmap)
install.packages("ggplot2")
library(ggplot2)
install.packages("plyr")
library(plyr)
install.packages("sqldf")
library(sqldf)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("cowplot")
library(cowplot)
syracuse_map <- get_map(unlist(geocode("Syracuse University, Syracuse, NY")), zoom = 15)
city_map <- get_map(unlist(geocode("Syracuse University, Syracuse, NY")), zoom = 13)
shapeData <- readOGR("../data/street_shapefile/",'streets')
shp <- spTransform(shapeData, CRS("+proj=longlat +ellps=GRS80"))
shape_data <- shp@data

all_ratings = NULL
for (year in seq(from=2000, to=2014)) {
  ratings <- read.csv(paste0("H:/civicdatahackathon/roadRatingNew", year, '.csv'), 
                      stringsAsFactors = FALSE)
  ratings$ratings_year <- year
  if (is.null(df)) {
    all_ratings = ratings
  } else {
    all_ratings <- rbind(all_ratings, ratings)  
  }
}


all_ratings$overall <- as.numeric(all_ratings$overall)
all_ratings <- subset(all_ratings, !is.na(overall))

#all_data <- merge(shape_data, 
 #                 all_ratings,
  #                by.x = "STREET_ID", 
   #               by.y = "streetID")
all_data <- all_ratings

all_data$dateLastOverlay <- as.numeric(all_data$dateLastOverlay)
all_data$dateLastOverlay[is.na(all_data$dateLastOverlay)] <- 1985
all_data$pavement <- as.numeric(all_data$pavement)
all_data$length <- as.numeric(all_data$length)
all_data$overall[all_data$overall>10] <- 10
all_data$crack <- as.numeric(all_data$crack)
all_data$pavement <- as.numeric(all_data$pavement)
all_data$patch <- as.numeric(all_data$patch)

ratings.rf <- lm(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
                 + classnum + pavement ,data=all_data)

create_data <- function(year) {
  shp@data <- merge(shape_data, 
                    subset(all_ratings, ratings_year == year), 
                    by.x = "STREET_ID", 
                    by.y = "streetID")
  
  
  data_fort <- fortify(shp)
  
  
  shp.df <- data.frame(id=rownames(shp@data),
                       shp@data, stringsAsFactors=F)
  
  data_merged <- join(data_fort, shp.df, by='id')
  data_merged <- subset(data_merged, !is.na(overall))
  
  data_merged$dateLastOverlay <- as.numeric(data_merged$dateLastOverlay)
  data_merged$dateLastOverlay[is.na(data_merged$dateLastOverlay)] <- 1985
  data_merged$pavement <- as.numeric(data_merged$pavement)
  data_merged$length <- as.numeric(data_merged$length)
  data_merged
}