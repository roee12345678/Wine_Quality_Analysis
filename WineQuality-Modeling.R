file_path <- "C:/Users/Nofit/Desktop/רואי/לימודים רואי/שנה ג/סמסטר ב/R/project/WineQuality-WhiteAndRedWine.csv"
wine_data <- read.csv(file_path)
library(tidyverse)
#install.packages("wine_data")
####modeling - Decision Trees
install.packages("randomForest")
install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
##################################
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)
###################################
model_data <- wine_data %>%
  select(excellent_good_bad,fixed.acidity,volatile.acidity,citric.acid,residual.sugar,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,density,pH,sulphates,alcohol)

set.seed(123)
train_index <- createDataPartition(model_data$excellent_good_bad, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]
train_data$excellent_good_bad <- as.factor(train_data$excellent_good_bad)
test_data$excellent_good_bad  <- as.factor(test_data$excellent_good_bad)

rf_model <- randomForest(
  formula = excellent_good_bad ~ .,
  data = train_data,
  importance = TRUE,
  ntree = 500
)
predictions <- predict(rf_model, newdata = test_data)
confusionMatrix(predictions, test_data$excellent_good_bad)
varImpPlot(rf_model)
importance(rf_model)
mean(predictions == test_data$excellent_good_bad)

#################################using the model
new_wine <- data.frame(
  fixed.acidity = 5,
  volatile.acidity=0.43,
  citric.acid=0.35,
  residual.sugar=7.1,
  chlorides = 0.08,
  free.sulfur.dioxide = 40,
  total.sulfur.dioxide = 100,
  density = 1.5,
  pH = 3.3,
  sulphates = 0.7,
  alcohol = 14
)
predict(rf_model, newdata = new_wine, type = "prob")

#################################################make the model better accuracy
model_data <- wine_data %>%
  select(excellent_good_bad, volatile.acidity,citric.acid,residual.sugar,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,density,pH,sulphates,alcohol)
set.seed(123)
train_index <- createDataPartition(model_data$excellent_good_bad, p = 0.9, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]
train_data$excellent_good_bad <- as.factor(train_data$excellent_good_bad)
test_data$excellent_good_bad  <- as.factor(test_data$excellent_good_bad)

rf_model <- randomForest(
  formula = excellent_good_bad ~ .,
  data = train_data,
  importance = TRUE,
  ntree = 300,
  classwt = c("bad" = 1, "good" = 1, "excellent" = 2.8)
)
predictions <- predict(rf_model, newdata = test_data)
confusionMatrix(predictions, test_data$excellent_good_bad)
varImpPlot(rf_model)
importance(rf_model)
mean(predictions == test_data$excellent_good_bad)
plot(rf_model)

#################################using the model again
new_wine <- data.frame(
  fixed.acidity = 5,
  volatile.acidity=0.43,
  citric.acid=0.35,
  residual.sugar=7.1,
  chlorides = 0.08,
  free.sulfur.dioxide = 40,
  total.sulfur.dioxide = 100,
  density = 1.5,
  pH = 3.3,
  sulphates = 0.7,
  alcohol = 14
)
predict(rf_model, newdata = new_wine, type = "prob")




