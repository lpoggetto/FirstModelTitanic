#Working Directory
setwd("~/R/Titanic")

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

#importing data
train_data <- read.csv2("train.csv", sep = ",", stringsAsFactors = FALSE)
test_data <- read.csv2("test.csv", sep = ",", stringsAsFactors = FALSE)

#Creating columns to make it easier to separate the data
test_data$Survived <- NA
train_data$train <- TRUE
test_data$train <- FALSE

#Creating one dataframe that contains all information
full_data <- bind_rows(train_data, test_data)

#Exploratory Analysis

#checking all data
str(full_data)

#number of lines and columns
dim(full_data)

#checking for unique values
lapply(full_data, function(x) length(unique(x)))

#including the mode of embarked
full_data[full_data$Embarked=='', "Embarked"] <- 'S'

#creating a median variable for age and applying on the data set to remove missing values
age_median <- median(full_data$Age, na.rm = TRUE)

full_data[full_data$Age == '', 'Age'] <- age_median

#applying the median creates a concentration
full_data$Age <- as.numeric(full_data$Age)

qplot(full_data$Age, geom = 'histogram', xlab = 'Idade')

#Fare Median and applying it on data
fare_median <- median(full_data$Fare, na.rm = TRUE)

full_data[full_data$Fare=='', 'Fare'] <- fare_median

full_data$Fare <- as.numeric(full_data$Fare)

qplot(full_data$Fare, geom = 'histogram')

#categorical casting
full_data$Pclass <- as.factor(full_data$Pclass)
full_data$Sex <- as.factor(full_data$Sex)
full_data$Embarked <-as.factor(full_data$Embarked)

str(full_data)
#Separating the clean data into train and test dataframes
train_data <- full_data[full_data$train == TRUE,]
train_data$Survived <- as.factor(train_data$Survived)

test_data <- full_data[full_data$train == FALSE,]


#creating a Formula using the variables that are relevant
survived_equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived_formula <- as.formula(survived_equation)

#Creating a model
titanic_model <- randomForest(formula = survived_formula, data = train_data, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(test_data))

#Selecting Features
features_equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

#applying predict
Survived <- predict(titanic_model, newdata = test_data)

#preparing the submission
PassengerId <- test_data$PassengerId
output_df <- as.data.frame(PassengerId)
output_df$Survived <- Survived
write.csv(output_df, file = "kaggle_submission.csv", row.names = FALSE)

