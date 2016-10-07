#Civic Data Hackathon
install.packages("randomForest")
library(randomForest)

roadRatings2015 <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/civicdatahackathon/roadRatingNew2015.csv')
roadRatings2015 <- na.omit(roadRatings2015)

ratings.rf <- randomForest(overall ~ streetType + crack + patch + dateLastOverlay + flushOilNum
                           + classnum + pavement + NumofYrs ,data=roadRatings2015)

ratings.rf <- randomForest(overall ~ streetType + crack + length + width + patch + dateLastOverlay + flushOilNum
                           + classnum + pavement + NumofYrs ,data=roadRatings2015)

str(roadRatings2015)
(importance(ratings.rf))
summary(ratings.rf)

summary(roadRatings2015)
dim(roadRatings2015)

p <- predict(rfm,test)
