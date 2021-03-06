#Civic Data Hackathon
install.packages("randomForest")
library(randomForest)
library(lubridate)

roadRatings <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/civicdatahackathon/roadRatingAll.csv', na.strings=c("","NA"))
roadRatings <- roadRatings[4866:77821,]
roadRatings = NULL
for (year in seq(from=2000, to=2015)) {
  ratings <- read.csv(paste0("C:/Users/SOHAM/Documents/civicdatahackathon/roadRatingNew", year, '.csv'), 
                      stringsAsFactors = FALSE)
  ratings$ratings_year <- year
  if (is.null(df)) {
    roadRatings = ratings
  } else {
    roadRatings <- rbind(roadRatings, ratings)  
  }
}


#roadRatings$dateRated <- na.omit(roadRatings$dateRated)
#roadRatings$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings$length <- as.numeric(roadRatings$length)

roadRatings$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings$NumofYrs[is.na(roadRatings$NumofYrs)] <- 0
str(roadRatings)

unique(roadRatings$overall)
roadRatings$overall <- as.numeric(as.character(roadRatings$overall))

summary(roadRatings)
cc <- is.na(roadRatings$overall)
m <- which(cc == ("TRUE"))
roadRatings <- roadRatings[-m,]

roadRatings <- roadRatings[-which(roadRatings$overall == 44 | roadRatings$overall == 19),]

roadRatings <- roadRatings[!is.na(roadRatings$crack),]
roadRatings <- roadRatings[!is.na(roadRatings$patch),]
roadRatings <- roadRatings[!is.na(roadRatings$pavement),]


roadRatings$pavement <- as.numeric(roadRatings$pavement)
roadRatings$crack <- as.numeric(roadRatings$crack)
roadRatings$patch <- as.numeric(roadRatings$patch)
unique(roadRatings$overall)


str(roadRatings)
summary(roadRatings)

unique(roadRatings$overall)

write.csv(x = roadRatings,file = "roadRatings00_15.csv")

#ratings.rf <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
#                           + classnum + pavement ,data=roadRatings)

install.packages("rpart")
library(rpart)

roadRatings.impute <- rfImpute(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum, roadRatings)
roadRatings.rpart <- rpart(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum, roadRatings)

roadRating.rpart.predict <- predict(ratings.rf,roadRatings2015)

roadRatings2015.pred <- data.frame(roadRating.rpart.predict)

mean(roadRatings2015.pred$roadRating.rpart.predict,na.rm = TRUE)

ratings.rf <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum, data=roadRatings)
                           #+ classnum + pavement,



roadRatings2015 <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/civicdatahackathon/roadRatingNew2015.csv', na.strings=c("","NA"))



roadRatings2015$length <- as.numeric(roadRatings2015$length)

roadRatings2015$NumofYrs <- as.numeric(roadRatings2015$NumofYrs)
roadRatings2015$NumofYrs[is.na(roadRatings2015$NumofYrs)] <- 0
str(roadRatings2015)

unique(roadRatings2015$overall)
summary(roadRatings2015)
cc <- is.na(roadRatings2015$overall)
m <- which(cc == ("TRUE"))
roadRatings2015 <- roadRatings2015[-m,]
roadRatings <- roadRatings[-m,]




roadRatings2015 <- roadRatings2015[!is.na(roadRatings2015$crack),]
roadRatings2015 <- roadRatings2015[!is.na(roadRatings2015$patch),]
roadRatings2015 <- roadRatings2015[!is.na(roadRatings2015$pavement),]

roadRatings2015$pavement <- as.numeric(roadRatings2015$pavement)
roadRatings2015$crack <- as.numeric(roadRatings2015$crack)
roadRatings2015$patch <- as.numeric(roadRatings2015$patch)



roadRatings2015$model1 <- predict(ratings.rf,roadRatings2015,type = "response")
mean(roadRatings2015$model1)
roadRatings2015$overall <- round(roadRatings2015$overall)
roadRatings2015$model1 <- round(roadRatings2015$model1)

confusionMatrix(roadRatings2015$overall,roadRatings2015$model1)
str(roadRatings2015)



# Linear Regression



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

unique(roadRatings$overall)
roadRatings$overall <- as.numeric(as.character(roadRatings$overall))

summary(roadRatings)
cc <- is.na(roadRatings$overall)
m <- which(cc == ("TRUE"))
roadRatings <- roadRatings[-m,]

roadRatings <- roadRatings[-which(roadRatings$overall == 44 | roadRatings$overall == 19),]

roadRatings <- roadRatings[!is.na(roadRatings$crack),]
roadRatings <- roadRatings[!is.na(roadRatings$patch),]
roadRatings <- roadRatings[!is.na(roadRatings$pavement),]


roadRatings$pavement <- as.numeric(roadRatings$pavement)
roadRatings$crack <- as.numeric(roadRatings$crack)
roadRatings$patch <- as.numeric(roadRatings$patch)
unique(roadRatings$overall)


str(roadRatings)
summary(roadRatings)

ratings.rf <- lm(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
                           + classnum + pavement ,data=roadRatings)

ratings.rf <- lm(overall ~ crack  + patch + dateLastOverlay + flushOilNum + NumofYrs ,data=roadRatings)

summary(ratings.rf)


roadRatings2015 <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/civicdatahackathon/roadRatingNew2015.csv', na.strings=c("","NA"))



roadRatings2015$length <- as.numeric(roadRatings2015$length)

roadRatings2015$NumofYrs <- as.numeric(roadRatings2015$NumofYrs)
roadRatings2015$NumofYrs[is.na(roadRatings2015$NumofYrs)] <- 0
str(roadRatings2015)

unique(roadRatings2015$overall)
summary(roadRatings2015)
cc <- is.na(roadRatings2015$overall)
m <- which(cc == ("TRUE"))
roadRatings2015 <- roadRatings2015[-m,]
roadRatings <- roadRatings[-m,]




roadRatings2015 <- roadRatings2015[!is.na(roadRatings2015$crack),]
roadRatings2015 <- roadRatings2015[!is.na(roadRatings2015$patch),]
roadRatings2015 <- roadRatings2015[!is.na(roadRatings2015$pavement),]

roadRatings2015$pavement <- as.numeric(roadRatings2015$pavement)
roadRatings2015$crack <- as.numeric(roadRatings2015$crack)
roadRatings2015$patch <- as.numeric(roadRatings2015$patch)



roadRatings2015$model1 <- predict(ratings.rf,roadRatings2015)
mean(roadRatings$overall)
mean(roadRatings$model1)
mean(roadRatings2015$model1)
roadRatings2015$overall <- round(roadRatings2015$overall)
roadRatings2015$model1 <- round(roadRatings2015$model1)

confusionMatrix(roadRatings2015$overall,roadRatings2015$model1)
str(roadRatings2015)

