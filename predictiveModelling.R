#Civic Data Hackathon
install.packages("randomForest")
library(randomForest)
library(lubridate)

roadRatings <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/civicdatahackathon/roadRatingAll.csv', na.strings=c("","NA"))
roadRatings <- roadRatings[1:72956,]


#roadRatings$dateRated <- na.omit(roadRatings$dateRated)
#roadRatings$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings$length <- as.numeric(roadRatings$length)

roadRatings$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings$NumofYrs[is.na(roadRatings$NumofYrs)] <- 0
str(roadRatings)

unique(roadRatings$NumofYrs)
summary(roadRatings)
cc <- is.na(roadRatings$overall)
m <- which(cc == ("TRUE"))
roadRatings <- roadRatings[-m,]

roadRatings <- roadRatings[!is.na(roadRatings$crack),]
roadRatings <- roadRatings[!is.na(roadRatings$patch),]
roadRatings <- roadRatings[!is.na(roadRatings$pavement),]

roadRatings$pavement <- as.numeric(roadRatings$pavement)
roadRatings$crack <- as.numeric(roadRatings$crack)
roadRatings$patch <- as.numeric(roadRatings$patch)
roadRatings$overall <- as.numeric(roadRatings$overall)

str(roadRatings)
summary(roadRatings)

ratings.rf <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
                           + classnum + pavement + NumofYrs ,data=roadRatings)


#2015 Ratings data
roadRatings2015 <- na.omit(roadRatings2015)
roadRatings2015$NumofYrs <- as.numeric(roadRatings2015$NumofYrs)

#2015 RF model
ratings2015.rf <- randomForest(overall ~ crack + patch + NumofYrs ,data=roadRatings2015)
ratings2015.rf <- randomForest(overall ~ streetType + crack + length + width + patch + dateLastOverlay + flushOilNum
                           + classnum + pavement + NumofYrs ,data=roadRatings2015)

str(roadRatings2015)
(importance(ratings.rf))
summary(ratings.rf)

summary(roadRatings2015)
dim(roadRatings2015)

roadRatings2014 <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/civicdatahackathon/roadRatingNew2014.csv')
roadRatings2014 <- na.omit(roadRatings2014)

roadRatins2014.train <- roadRatings2014[c('crack','patch','NumofYears')]

names(roadRatings2014)[names(roadRatings2014) == "NumofYears"] <- "NumofYrs"

roadRatings2014$NumofYrs <- as.numeric(roadRatings2014$NumofYrs)

roadRatings2014.pred$model1 <- predict(ratings.rf,roadRatings2014,type = "response")

roadRatings2014.pred$model2 <- predict(ratings.rf,roadRatings2014,type = "response")

#roadRatings2014.pred <- ceiling(roadRatings2014.pred)
roadRatings2014.pred$actualValue <- roadRatings2014$overall

roadRatings2014.pred$mse <- sqrt((roadRatings2014.pred$actualValue - roadRatings2014.pred$model1)^2)

roadRatings2014.pred$mse2 <- sqrt((roadRatings2014.pred$actualValue - roadRatings2014.pred$model2)^2)

mean(roadRatings2014.pred$mse2,na.rm = TRUE)

sum(roadRatings2014.pred$mse2,na.rm = TRUE)/length(roadRatings2014.pred$mse2)

sum(roadRatings2014.pred$mse,na.rm = TRUE)/length(roadRatings2014.pred$mse)

roadRatings2014.pred$roundedPredictedValue1 <- round(roadRatings2014.pred$model1)
roadRatings2014.pred$roundedPredictedValue2 <- round(roadRatings2014.pred$model2)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(roadRatings2014.pred$roundedPredictedValue2,roadRatings2014.pred$actualValue)

table(roadRatings2014.pred$roundedPredictedValue2,roadRatings2014.pred$actualValue)

table(roadRatings2014.pred$roundedPredictedValue1)

table(roadRatings2014.pred$actualValue)

table(roadRatings2014.pred$roundedPredictedValue2)

str(roadRatings2014)
str(roadRatings2015)
