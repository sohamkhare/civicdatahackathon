
roadRatings2014 <- read.csv('//hd.ad.syr.edu/03/e1c9c9/Documents/civicdatahackathon/roadRatingNew2014.csv', na.strings=c("","NA"))



#roadRatings$dateRated <- na.omit(roadRatings$dateRated)
#roadRatings$NumofYrs <- as.numeric(roadRatings$NumofYrs)
roadRatings2014$length <- as.numeric(roadRatings2014$length)

names(roadRatings2014)[names(roadRatings2014) == "NumofYears"] <- "NumofYrs"
roadRatings2014$NumofYrs <- as.numeric(roadRatings2014$NumofYrs)
roadRatings2014$NumofYrs[is.na(roadRatings2014$NumofYrs)] <- 0
str(roadRatings2014)

unique(roadRatings2014$overall)
roadRatings2014$overall <- as.numeric(as.character(roadRatings2014$overall))

summary(roadRatings2014)
cc <- is.na(roadRatings2014$overall)
m <- which(cc == ("TRUE"))
roadRatings2014 <- roadRatings2014[-m,]



roadRatings2014 <- roadRatings2014[!is.na(roadRatings2014$crack),]
roadRatings2014 <- roadRatings2014[!is.na(roadRatings2014$patch),]
roadRatings2014 <- roadRatings2014[!is.na(roadRatings2014$pavement),]


roadRatings2014$pavement <- as.numeric(roadRatings2014$pavement)
roadRatings2014$crack <- as.numeric(roadRatings2014$crack)
roadRatings2014$patch <- as.numeric(roadRatings2014$patch)
unique(roadRatings2014$overall)


str(roadRatings2014)
summary(roadRatings2014$overall)
mean(roadRatings2014$overall)

ratings.rf <- lm(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum
                 + classnum + pavement + NumofYrs ,data=roadRatings2014)

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
roadRatings2015$overall <- roadRatings2015$overall
roadRatings2015$model1 <- round(roadRatings2015$model1)



unique(roadRatings2015$overall)
unique(roadRatings2015$model1)
confusionMatrix(roadRatings2015$overall,roadRatings2015$model1)
str(roadRatings2015)

which.max(roadRatings2015$model1)


