#Part 1

roadRatings_00_11 = data.frame()
for (i in seq(from=2000, to=2011)) {
  yearly_data <- read.csv(paste0("/Users/Pankaj/civicdatahackathon/roadRatingNew", i, '.csv'), 
                      stringsAsFactors = FALSE)
  roadRatings_00_11 <- rbind(roadRatings_00_11, yearly_data)
}

roadRatings_00_11


#roadRatings_00_11$dateRated <- na.omit(roadRatings$dateRated)
#roadRatings_00_11$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings_00_11$length <- as.numeric(roadRatings_00_11$length)

roadRatings_00_11$NumofYrs <- as.numeric(roadRatings_00_11$NumofYrs)
roadRatings_00_11$NumofYrs[is.na(roadRatings_00_11$NumofYrs)] <- 0
str(roadRatings_00_11)

unique(roadRatings_00_11$overall)
roadRatings_00_11$overall <- as.numeric(as.character(roadRatings_00_11$overall))

summary(roadRatings_00_11)
cc <- is.na(roadRatings_00_11$overall)
m <- which(cc == ("TRUE"))
roadRatings_00_11 <- roadRatings_00_11[-m,]

roadRatings_00_11 <- roadRatings_00_11[-which(roadRatings_00_11$overall == 44 | roadRatings_00_11$overall == 19),]

roadRatings_00_11 <- roadRatings_00_11[!is.na(roadRatings_00_11$crack),]
roadRatings_00_11 <- roadRatings_00_11[!is.na(roadRatings_00_11$patch),]
roadRatings_00_11 <- roadRatings_00_11[!is.na(roadRatings_00_11$pavement),]


roadRatings_00_11$pavement <- as.numeric(as.character(roadRatings_00_11$pavement))
roadRatings_00_11$crack <- as.numeric(roadRatings_00_11$crack)
roadRatings_00_11$patch <- as.numeric(roadRatings_00_11$patch)
unique(roadRatings_00_11$overall)


str(roadRatings_00_11)
summary(roadRatings_00_11)

unique(roadRatings_00_11$overall)
unique(roadRatings_00_11$pavement)

write.csv(roadRatings_00_11, file = "roadRatings_00_11.csv", row.names = TRUE)


#roadRatings_00_11$overall <- as.factor(roadRatings_00_11$overall)
#ratings.rf <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
#                           + classnum + pavement ,data=roadRatings)

install.packages("rpart")
library(rpart)


#roadRatings.impute <- rfImpute(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum, roadRatings)
#roadRatings_00_11.rpart <- rpart(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
roadRatings_00_11.rpart <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
#roadRatings_00_11.rpart <- lm(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
#roadRatings_00_11.rpart <- rpart(overall ~ ., roadRatings_00_11)

summary(roadRatings_00_11.rpart)

roadRatings_12_15 = data.frame()
for (i in seq(from=2012, to=2015)) {
  yearly_data <- read.csv(paste0("/Users/Pankaj/civicdatahackathon/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_12_15 <- rbind(roadRatings_12_15, yearly_data)
}



roadRatings_12_15$predict <- round(predict(roadRatings_00_11.rpart,roadRatings_12_15))
#roadRatings_12_15$predict <- predict(roadRatings_00_11.rpart,roadRatings_12_15)
#roadRatings_12_15.pred <- data.frame(roadRating.rpart.predict)

mean(roadRatings_12_15$predict,na.rm = TRUE)

mean(roadRatings_12_15$overall,na.rm = TRUE)
table(roadRatings_12_15$overall,roadRatings_12_15$predict)

write.csv(roadRatings_12_15, file = "roadRatings_12_15.csv", row.names = TRUE)

library(caret)
#confusionMatrix(roadRatings_12_15$overall,roadRatings_12_15$predict)

unique(roadRatings_12_15$predict)
unique(roadRatings_12_15$overall)

plot(roadRatings_12_15$overall, type = "l", col = "red", lwd=2
     , main = "Regional sales over time", adj = 0)

hist(roadRatings_12_15$predict,xlim = c(0,10))
hist(roadRatings_12_15$overall,xlim = c(0,10))


# Part 2

roadRatings_14 = data.frame()
for (i in seq(from=2014, to=2014)) {
  yearly_data <- read.csv(paste0("/Users/Pankaj/civicdatahackathon/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_14 <- rbind(roadRatings_14, yearly_data)
}

roadRatings_14


#roadRatings_00_11$dateRated <- na.omit(roadRatings$dateRated)
#roadRatings_00_11$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings_14$length <- as.numeric(roadRatings_14$length)

roadRatings_14$NumofYrs <- as.numeric(roadRatings_14$NumofYrs)
roadRatings_14$NumofYrs[is.na(roadRatings_14$NumofYrs)] <- 0
str(roadRatings_14)

unique(roadRatings_14$overall)
roadRatings_14$overall <- as.numeric(as.character(roadRatings_14$overall))

summary(roadRatings_14)
cc <- is.na(roadRatings_14$overall)
m <- which(cc == ("TRUE"))
roadRatings_14 <- roadRatings_14[-m,]

#roadRatings_14 <- roadRatings_14[-which(roadRatings_14$overall == 44 | roadRatings_14$overall == 19),]

roadRatings_14 <- roadRatings_14[!is.na(roadRatings_14$crack),]
roadRatings_14 <- roadRatings_14[!is.na(roadRatings_14$patch),]
roadRatings_14 <- roadRatings_14[!is.na(roadRatings_14$pavement),]


roadRatings_14$pavement <- as.numeric(as.character(roadRatings_14$pavement))
roadRatings_14$crack <- as.numeric(roadRatings_14$crack)
roadRatings_14$patch <- as.numeric(roadRatings_14$patch)
unique(roadRatings_14$overall)


str(roadRatings_14)
summary(roadRatings_14)

unique(roadRatings_14$overall)
unique(roadRatings_14$pavement)

write.csv(roadRatings_14, file = "roadRatings_14.csv", row.names = TRUE)


#roadRatings_00_11$overall <- as.factor(roadRatings_00_11$overall)
#ratings.rf <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
#                           + classnum + pavement ,data=roadRatings)

#install.packages("rpart")
#library(rpart)


#roadRatings.impute <- rfImpute(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum, roadRatings)
#roadRatings_00_11.rpart <- rpart(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
roadRatings_14.rpart <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_14)
#roadRatings_00_11.rpart <- lm(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
#roadRatings_00_11.rpart <- rpart(overall ~ ., roadRatings_00_11)

summary(roadRatings_14.rpart)

roadRatings_15 = data.frame()
for (i in seq(from=2015, to=2015)) {
  yearly_data <- read.csv(paste0("/Users/Pankaj/civicdatahackathon/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_15 <- rbind(roadRatings_15, yearly_data)
}



roadRatings_15$predict <- round(predict(roadRatings_14.rpart,roadRatings_15))
#roadRatings_12_15$predict <- predict(roadRatings_00_11.rpart,roadRatings_12_15)
#roadRatings_12_15.pred <- data.frame(roadRating.rpart.predict)

mean(roadRatings_15$predict,na.rm = TRUE)

mean(roadRatings_15$overall,na.rm = TRUE)
table(roadRatings_15$overall,roadRatings_15$predict)

write.csv(roadRatings_15, file = "roadRatings_15.csv", row.names = TRUE)

library(caret)
#confusionMatrix(roadRatings_12_15$overall,roadRatings_12_15$predict)

unique(roadRatings_15$predict)
unique(roadRatings_15$overall)

plot(roadRatings_15$overall, type = "l", col = "red", lwd=2
     , main = "Regional sales over time", adj = 0)

hist(roadRatings_15$predict,xlim = c(0,10))
hist(roadRatings_15$overall,xlim = c(0,10))

# Part 3

roadRatings_00_14 = data.frame()
for (i in seq(from=2000, to=2014)) {
  yearly_data <- read.csv(paste0("/Users/Pankaj/civicdatahackathon/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_00_14 <- rbind(roadRatings_00_14, yearly_data)
}

roadRatings_00_14


#roadRatings_00_11$dateRated <- na.omit(roadRatings$dateRated)
#roadRatings_00_11$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings_00_14$length <- as.numeric(roadRatings_00_14$length)

roadRatings_00_14$NumofYrs <- as.numeric(roadRatings_00_14$NumofYrs)
roadRatings_00_14$NumofYrs[is.na(roadRatings_00_14$NumofYrs)] <- 0
str(roadRatings_00_14)

unique(roadRatings_00_14$overall)
roadRatings_00_14$overall <- as.numeric(as.character(roadRatings_00_14$overall))

summary(roadRatings_00_14)
cc <- is.na(roadRatings_00_14$overall)
m <- which(cc == ("TRUE"))
roadRatings_00_14 <- roadRatings_00_14[-m,]

roadRatings_00_14 <- roadRatings_00_14[-which(roadRatings_00_14$overall == 44 | roadRatings_00_14$overall == 19),]

roadRatings_00_14 <- roadRatings_00_14[!is.na(roadRatings_00_14$crack),]
roadRatings_00_14 <- roadRatings_00_14[!is.na(roadRatings_00_14$patch),]
roadRatings_00_14 <- roadRatings_00_14[!is.na(roadRatings_00_14$pavement),]


roadRatings_00_14$pavement <- as.numeric(as.character(roadRatings_00_14$pavement))
roadRatings_00_14$crack <- as.numeric(roadRatings_00_14$crack)
roadRatings_00_14$patch <- as.numeric(roadRatings_00_14$patch)
unique(roadRatings_00_14$overall)


str(roadRatings_00_14)
summary(roadRatings_00_14)

unique(roadRatings_00_14$overall)
unique(roadRatings_00_14$pavement)

write.csv(roadRatings_00_14, file = "roadRatings_00_14.csv", row.names = TRUE)


#roadRatings_00_11$overall <- as.factor(roadRatings_00_11$overall)
#ratings.rf <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
#                           + classnum + pavement ,data=roadRatings)

install.packages("rpart")
library(rpart)


#roadRatings.impute <- rfImpute(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum, roadRatings)
#roadRatings_00_11.rpart <- rpart(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
roadRatings_00_14.rpart <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_14)
#roadRatings_00_11.rpart <- lm(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
#roadRatings_00_11.rpart <- rpart(overall ~ ., roadRatings_00_11)

summary(roadRatings_00_14.rpart)

roadRatings_15_2 = data.frame()
for (i in seq(from=2015, to=2015)) {
  yearly_data <- read.csv(paste0("/Users/Pankaj/civicdatahackathon/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_15_2 <- rbind(roadRatings_15_2, yearly_data)
}



roadRatings_15_2$predict <- round(predict(roadRatings_00_14.rpart,roadRatings_15_2))
#roadRatings_12_15$predict <- predict(roadRatings_00_11.rpart,roadRatings_12_15)
#roadRatings_12_15.pred <- data.frame(roadRating.rpart.predict)

mean(roadRatings_15_2$predict,na.rm = TRUE)

mean(roadRatings_15_2$overall,na.rm = TRUE)
table(roadRatings_15_2$overall,roadRatings_15_2$predict)

write.csv(roadRatings_15_2, file = "roadRatings_15_2.csv", row.names = TRUE)

library(caret)
#confusionMatrix(roadRatings_12_15$overall,roadRatings_12_15$predict)

unique(roadRatings_15_2$predict)
unique(roadRatings_15_2$overall)

plot(roadRatings_15_2$overall, type = "l", col = "red", lwd=2
     , main = "Regional sales over time", adj = 0)

hist(roadRatings_15_2$predict,xlim = c(0,10))
hist(roadRatings_15_2$overall,xlim = c(0,10))


