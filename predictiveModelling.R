#Civic Data Hackathon
install.packages("randomForest")
library(randomForest)

roadRatings2015 <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/Downloads/RoadRatings2015.csv')
ratings.rf <- randomForest(overall ~ streetType + crack + patch + dateLastOverlay + flushOil + class + pavement ,data=roadRatings2015)

str(roadRatings2015)
(importance(ratings.rf))
summary(ratings.rf)
roadRatings2015 <- na.omit(roadRatings2015)
summary(roadRatings2015)
dim(roadRatings2015)
