file_path <- "C:/Users/Nofit/Desktop/רואי/לימודים רואי/שנה ג/סמסטר ב/R/project/WineQuality-WhiteAndRedWine.csv"
wine_data <- read.csv(file_path)
library(tidyverse)
#install.packages("wine_data")
library(modelr)
#install.packages("wine_data")
################Functions
################Function1 - insert a quality and get the types number
count_wines_by_type <- function(q) {
  filtered <- wine_data[wine_data$quality == q, ]
  counts <- table(filtered$type)
  return(counts)
}
count_wines_by_type(5) ###trying Function1
################Function2 - insert a quality, type and alcohol level and get the wine's number that fit
wine_data$excellent_good_bad <- ifelse(
  wine_data$quality > 6, "excellent",
  ifelse(wine_data$quality <= 5, "bad", "good")
)
count_by_quality_typt_alcoholLevel <- function(q,t,al){
  filtered <- wine_data[wine_data$quality ==q &wine_data$type ==t & wine_data$alcohol_level == al, ]
  return(nrow(filtered))
}
count_by_quality_typt_alcoholLevel(8,"white","Medium") ###trying Function2
#############################Transformations
#####A
wine_data$excellent_good_bad <- ifelse(
  wine_data$quality > 6, "excellent",
  ifelse(wine_data$quality <= 5, "bad", "good")
)
prop.table(table(wine_data$excellent_good_bad))###the proportion is quite good
View(wine_data)
######B
wine_data$alcohol_level <- cut(wine_data$alcohol,
                               breaks = c(0,10,11,20),
                               labels = c("Low","Medium","High"))
table(wine_data$alcohol_level)  
prop.table(table(wine_data$alcohol_level))###the proportion is quite good
View(wine_data) ### tring TransformationB
#######c
wine_data$next_level_score <- 
  (2 * wine_data$alcohol) + 
  (1 / (wine_data$volatile.acidity + 0.01)) + 
  (0.8 * wine_data$sulphates) +
  (0.5 * wine_data$citric.acid) -
  (1.5 * wine_data$density) -
  (1 * wine_data$chlorides)
View(wine_data) ### tring TransformationB
###########################calculations
### measure 1
sugar_acid_ratio <- wine_data$residual.sugar/wine_data$fixed.acidity
### measure 2
alcohol_PH_ratio <- wine_data$alcohol/wine_data$pH
### measue 3
alcohol_density_ratio <- wine_data$alcohol/wine_data$density
### measure 4
Mean_quality<-mean(wine_data$quality)
###measure 5
acidity_score <- wine_data$fixed.acidity + 
  wine_data$volatile.acidity + 
  wine_data$citric.acid
###measure 6
aroma_score <- wine_data$sulphates + 
  wine_data$citric.acid - 
  wine_data$volatile.acidity
###measure 7
next_level_score <- 
  (2 * wine_data$alcohol) + 
  (1 / (wine_data$volatile.acidity + 0.01)) + 
  (0.8 * wine_data$sulphates) +
  (0.5 * wine_data$citric.acid) -
  (1.5 * wine_data$density) -
  (1 * wine_data$chlorides)
#############################factors
###factor 1
wine_data$type <-factor(wine_data$type)
summary(wine_data$type) ### ensuring factor1
###factor 2
wine_data$excellent_good_bad <- factor(wine_data$excellent_good_bad)
summary(wine_data$excellent_good_bad) ### ensuring factor2
prop.table(table(wine_data$excellent_good_bad)) ###the proportion is not bad
########################Exploratory
summary(wine_data)
str(wine_data)
var(wine_data)
colSums(is.na(wine_data)) ##there are not NA in the dataset
wine_data %>%
  select(type,alcohol_level, quality)
wine_data %>%
  count(quality)
######################Visualizations
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=excellent_good_bad,color=type)) ##ensuring that the equality is enough
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=alcohol_level,,color=type)) #ensuring that the equality is enough
#################
ggplot(data=wine_data)+
  geom_histogram(mapping = aes(x=quality,color=type),binwidth = 1)## looks like a normal distribution, but better only with white wine. the normal distribution is good for us to the learning model, and there are no abnormals
ggplot(data = wine_data)+
  geom_histogram(mapping = aes(x=pH,color=type),binwidth = 0.2) ## looks like a normal distribution
ggplot(data = wine_data)+
  geom_histogram(mapping = aes(x=sulphates,color=type)) ## looks like a normal distribution
ggplot(data = wine_data)+
  geom_histogram(mapping = aes(x=fixed.acidity,color=type)) ## looks like a normal distribution
ggplot(data=wine_data[wine_data$type=="white",])+
  geom_histogram(mapping = aes(x=quality),binwidth = 1)## better normal distribution
######## abnormals, we will not use them in our model
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=chlorides,color=type)) ## abnormal
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=volatile.acidity,color=type)) ## abnormal
###################################
ggplot(data = wine_data, aes(x = alcohol, y = quality)) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) ### in general, more alcohol - more quality
cor(wine_data$alcohol, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_line(mapping = aes(x=alcohol_density_ratio,y=quality)) ###in general, more alcohol-density ratio, more quality
cor(alcohol_density_ratio, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=alcohol_PH_ratio,y=quality)) ###in general, more alcohol-PH ratio, more quality
cor(alcohol_PH_ratio, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=acidity_score,y=pH)) ###in general, more acidity less pH
cor(acidity_score, wine_data$pH) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=volatile.acidity,y=quality)) ###in general, more volatile acidity, less quality
cor(acidity_score, wine_data$quality) ###checking the correlation
 ggplot(data=wine_data)+
   geom_smooth(mapping = aes(x=next_level_score,y=quality)) ###in general, more next level score, more qualitbalance_scorecore, wine_data$qualtion
 cor(next_level_score, wine_data$quality) ###checking the correlation,this is the best correlation with the quality thay I found 
 ggplot(data=wine_data)+
   geom_count(mapping = aes(x=next_level_score,y=quality)) ### it is interesting that the wines that got "9" in qiality, does not have the best score
 wine_data %>%
count(wine_data$next_level_score,quality) #numbers of wines to each "next_level_score"
##############################################################
############# Heat map, we can see that the alcohol level affects directly on the quality
 wine_data %>%
   mutate(
     alcohol_level = factor(alcohol_level, levels = c("Low", "Medium", "High")),
     excellent_good_bad = factor(excellent_good_bad, levels = c("excellent", "good", "bad"))
   ) %>%
   count(alcohol_level, excellent_good_bad) %>%
   ggplot(aes(x = alcohol_level, y = excellent_good_bad)) +
   geom_tile(aes(fill = n))
 ############################ using grid, visualized the connection between alcohol level and quality
 data_grid(alcohol_level) %>%
   add_predictions(mod)
 ggplot(wine_data, aes(x = alcohol_level)) +
   geom_point(aes(y = quality)) +
   geom_point(
     data = grid,
     aes(y = pred),
     color = "red",
     size = 4
   )
#############################################################################################
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
   select(excellent_good_bad, alcohol, sulphates, density,volatile.acidity,citric.acid,residual.sugar,free.sulfur.dioxide, pH,chlorides,fixed.acidity,total.sulfur.dioxide)
 
 set.seed(123)
 train_index <- createDataPartition(model_data$excellent_good_bad, p = 0.8, list = FALSE)
 train_data <- model_data[train_index, ]
 test_data  <- model_data[-train_index, ]
 
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
mean(predictions == test_data$excellent_good_bad) ## the accuracy of the model

#################################using the model
new_wine <- data.frame(
  alcohol = 12.5,
  sulphates = 0.65,
  density = 1.2,
  volatile.acidity = 0.21,
  citric.acid = 0.3,
  residual.sugar = 8.1,
  pH = 3.3,
  chlorides = 0.08,
  free.sulfur.dioxide = 30,
  fixed.acidity = 5,
  total.sulfur.dioxide = 100
)
predict(rf_model, newdata = new_wine, type = "prob") ##the predictation

importance(rf_model)
table(wine_data$excellent_good_bad)
#################################################make the model better accuracy
 model_data <- wine_data %>%
   select(excellent_good_bad,volatile.acidity,citric.acid,residual.sugar,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,density,pH,sulphates,alcohol)
 set.seed(123)
 train_index <- createDataPartition(model_data$excellent_good_bad, p = 0.9, list = FALSE)
 train_data <- model_data[train_index, ]
 test_data  <- model_data[-train_index, ]
 
 rf_model <- randomForest(
   formula = excellent_good_bad ~ .,
   data = train_data,
   importance = TRUE,
   ntree = 300,
   classwt = c("bad" = 1, "good" = 1, "excellent" = 3.2)
 )
 predictions <- predict(rf_model, newdata = test_data)
 confusionMatrix(predictions, test_data$excellent_good_bad)
 varImpPlot(rf_model)
importance(rf_model)
mean(predictions == test_data$excellent_good_bad) ##the new accuracy of the model, now its better
confusionMatrix(predictions, test_data$excellent_good_bad)

#################################using the model again
new_wine <- data.frame(
  alcohol = 12.5,
  sulphates = 0.65,
  density = 1.2,
  volatile.acidity = 0.21,
  citric.acid = 0.3,
  residual.sugar = 8.1,
  pH = 3.3,
  chlorides = 0.08,
  free.sulfur.dioxide = 30,
  fixed.acidity = 5,
  total.sulfur.dioxide = 100
)
predict(rf_model, newdata = new_wine, type = "prob") ##the predictation



importance(rf_model)
table(wine_data$excellent_good_bad)
var(model_data)
#install.packages("corrplot")
install.packages("corrplot")
library(corrplot)
corrplot(cor(model_data[, -1]), method = "color")
plot(rf_model)
cor(wine_data[, c( "fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol")])

