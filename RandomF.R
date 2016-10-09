#Predictive Modelling for the road ratings data

#New variables created:
# NumofYrs - The number of years from the last time the road was overlayed.
# NumofYrs = dateRated - dateLastOverlay



#Part 1 - Predictive modelling based on a predictive model generated for the training data
# from the year 2000 to 2011
# The model predicts the overall rating values for the year 2015

#Read the CSV files to generate a dataframe
roadRatings_00_11 = data.frame()
for (i in seq(from=2000, to=2011)) {
  yearly_data <- read.csv(paste0("./civicdatahackathon/data/roadRatingNew", i, '.csv'), 
                      stringsAsFactors = FALSE)
  roadRatings_00_11 <- rbind(roadRatings_00_11, yearly_data)
}

#Predictive modelling

#Convert all variables used for predictive modelling to numeric values
roadRatings_00_11$length <- as.numeric(roadRatings_00_11$length)
roadRatings_00_11$NumofYrs <- as.numeric(roadRatings_00_11$NumofYrs)
roadRatings_00_11$NumofYrs[is.na(roadRatings_00_11$NumofYrs)] <- 0

roadRatings_00_11$overall <- as.numeric(as.character(roadRatings_00_11$overall))

#Summary of ratings data frame
summary(roadRatings_00_11)
cc <- is.na(roadRatings_00_11$overall)
m <- which(cc == ("TRUE"))

roadRatings_00_11 <- roadRatings_00_11[-m,]

#Change erronous ratings to 10
roadRatings_00_11 <- roadRatings_00_11[-which(roadRatings_00_11$overall == 44 | roadRatings_00_11$overall == 19),]

#roadRatings_00_11 <- roadRatings_00_11[!is.na(roadRatings_00_11$crack),]
#roadRatings_00_11 <- roadRatings_00_11[!is.na(roadRatings_00_11$patch),]
#roadRatings_00_11 <- roadRatings_00_11[!is.na(roadRatings_00_11$pavement),]

roadRatings_00_11$pavement <- as.numeric(as.character(roadRatings_00_11$pavement))
roadRatings_00_11$crack <- as.numeric(roadRatings_00_11$crack)
roadRatings_00_11$patch <- as.numeric(roadRatings_00_11$patch)

#Write the dataframe to CSV file
write.csv(roadRatings_00_11, file = "roadRatings_00_11.csv", row.names = TRUE)


#roadRatings.impute <- rfImpute(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum, roadRatings)
#roadRatings_00_11.rpart <- rpart(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
#roadRatings_00_11.rpart <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
#roadRatings_00_11.rpart <- lm(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_00_11)
#roadRatings_00_11.rpart <- rpart(overall ~ ., roadRatings_00_11)


#Create dataframe for prediction
roadRatings_12_15 = data.frame()
for (i in seq(from=2012, to=2015)) {
  yearly_data <- read.csv(paste0("./civicdatahackathon/data/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_12_15 <- rbind(roadRatings_12_15, yearly_data)
}

#Prediction
roadRatings_12_15$predict <- round(predict(roadRatings_00_11.rpart,roadRatings_12_15))

#Mean of the predicted value for rating
mean(roadRatings_12_15$predict,na.rm = TRUE)

#Mean of the actual overall rating
mean(roadRatings_12_15$overall,na.rm = TRUE)

#
table(roadRatings_12_15$overall,roadRatings_12_15$predict)


#Write CSV for the predicted data
write.csv(roadRatings_12_15, file = "./civicdatahackathon/data/roadRatings_12_15.csv", row.names = TRUE)


#Unique ratings for Predicted values of rating
unique(roadRatings_12_15$predict)

#Unique ratings for actual values of rating
unique(roadRatings_12_15$overall)

#Histograms for the actual and predicted values
hist(roadRatings_12_15$predict,xlim = c(0,10))
hist(roadRatings_12_15$overall,xlim = c(0,10))



#Part 2 - Predictive modelling based on a predictive model generated for the training data
# from the year 2014
# The model predicts the overall rating values for the year 2015

#Create data frame for training data for Model 2 - For values from 2014
roadRatings_14 = data.frame()
for (i in seq(from=2014, to=2014)) {
  yearly_data <- read.csv(paste0("./civicdatahackathon/data/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_14 <- rbind(roadRatings_14, yearly_data)
}


#Predictive modelling

#Convert all variables used for predictive modelling to numeric values
roadRatings_14$length <- as.numeric(roadRatings_14$length)

roadRatings_14$NumofYrs <- as.numeric(roadRatings_14$NumofYrs)
#Remove NA values from number of years variable
roadRatings_14$NumofYrs[is.na(roadRatings_14$NumofYrs)] <- 0

#Convert overall ratings variable to numeric data type
roadRatings_14$overall <- as.numeric(as.character(roadRatings_14$overall))

#Remove NAs from overall ratings variable
summary(roadRatings_14)
cc <- is.na(roadRatings_14$overall)
m <- which(cc == ("TRUE"))
roadRatings_14 <- roadRatings_14[-m,]

#roadRatings_14 <- roadRatings_14[-which(roadRatings_14$overall == 44 | roadRatings_14$overall == 19),]

#roadRatings_14 <- roadRatings_14[!is.na(roadRatings_14$crack),]
#roadRatings_14 <- roadRatings_14[!is.na(roadRatings_14$patch),]
#roadRatings_14 <- roadRatings_14[!is.na(roadRatings_14$pavement),]


#Convert predictor variables to numeric type for prediction
roadRatings_14$pavement <- as.numeric(as.character(roadRatings_14$pavement))
roadRatings_14$crack <- as.numeric(roadRatings_14$crack)
roadRatings_14$patch <- as.numeric(roadRatings_14$patch)
#unique(roadRatings_14$overall)

#Write CSV of the dataframe
write.csv(roadRatings_14, file = "./civicdatahackathon/data/roadRatings_14.csv", row.names = TRUE)


#Predictive Modelling

#Random Forest
roadRatings_14.rf <- randomForest(overall ~ stretTypeNum + crack  + patch + dateLastOverlay + flushOilNum + classnum, roadRatings_14)

#Summary of RF
summary(roadRatings_14.rf)

#Predictor data for 2015
roadRatings_15 = data.frame()
for (i in seq(from=2015, to=2015)) {
  yearly_data <- read.csv(paste0("./civicdatahackathon/data/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_15 <- rbind(roadRatings_15, yearly_data)
}

#Predict values for overall rating
roadRatings_15$predict <- round(predict(roadRatings_14.rpart,roadRatings_15))

#Mean of the predicted values
mean(roadRatings_15$predict,na.rm = TRUE)

#Mean of the actual values
mean(roadRatings_15$overall,na.rm = TRUE)

#Write CSV for the predicted values
write.csv(roadRatings_15, file = "./civicdatahackathon/data/roadRatings_15.csv", row.names = TRUE)


#Unique values for predicted and actual ratings
unique(roadRatings_15$predict)
unique(roadRatings_15$overall)

#Histograms for predicted and actual values
hist(roadRatings_15$predict,xlim = c(0,10))
hist(roadRatings_15$overall,xlim = c(0,10))




#Part 3 - Predictive modelling based on a predictive model generated for the training data
# from the year 2000 to 2014
# The model predicts the overall rating values for the year 2015


#Create data frame for training data from the year 2000 to 2014
roadRatings_00_14 = data.frame()
for (i in seq(from=2000, to=2014)) {
  yearly_data <- read.csv(paste0("/Users/Pankaj/civicdatahackathon/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_00_14 <- rbind(roadRatings_00_14, yearly_data)
}


#Convert all variables used for predictive modelling to numeric values
roadRatings_00_14$length <- as.numeric(roadRatings_00_14$length)

roadRatings_00_14$NumofYrs <- as.numeric(roadRatings_00_14$NumofYrs)
roadRatings_00_14$NumofYrs[is.na(roadRatings_00_14$NumofYrs)] <- 0
roadRatings_00_14$overall <- as.numeric(as.character(roadRatings_00_14$overall))

#Remove NAs from the overall ratings
summary(roadRatings_00_14)
cc <- is.na(roadRatings_00_14$overall)
m <- which(cc == ("TRUE"))
roadRatings_00_14 <- roadRatings_00_14[-m,]

#Convert the ratings more than 10 to 10
roadRatings_00_14 <- roadRatings_00_14[-which(roadRatings_00_14$overall == 44 | roadRatings_00_14$overall == 19),]

#Convert the predictor variables to numeric data type
roadRatings_00_14$pavement <- as.numeric(as.character(roadRatings_00_14$pavement))
roadRatings_00_14$crack <- as.numeric(roadRatings_00_14$crack)
roadRatings_00_14$patch <- as.numeric(roadRatings_00_14$patch)

#Write CSV of the dataframe
write.csv(roadRatings_00_14, file = "roadRatings_00_14.csv", row.names = TRUE)



#Predictor data set for the year 2015
roadRatings_15_2 = data.frame()
for (i in seq(from=2015, to=2015)) {
  yearly_data <- read.csv(paste0("./data/roadRatingNew", i, '.csv'), 
                          stringsAsFactors = FALSE)
  roadRatings_15_2 <- rbind(roadRatings_15_2, yearly_data)
}



#Predictive Modelling
roadRatings_15_2$predict <- round(predict(roadRatings_00_14.rpart,roadRatings_15_2))

#Mean of the predicted values
mean(roadRatings_15_2$predict,na.rm = TRUE)

#Mean of the actual values
mean(roadRatings_15_2$overall,na.rm = TRUE)

#Write CSV of the dataframe
write.csv(roadRatings_15_2, file = "./data/roadRatings_15_2.csv", row.names = TRUE)

#Unique values of the overall and predicted ratings
unique(roadRatings_15_2$predict)
unique(roadRatings_15_2$overall)

#Histogram of the predicted and actual ratings
hist(roadRatings_15_2$predict,xlim = c(0,10))
hist(roadRatings_15_2$overall,xlim = c(0,10))


