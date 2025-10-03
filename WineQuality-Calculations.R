file_path <- "C:/Users/Nofit/Desktop/רואי/לימודים רואי/שנה ג/סמסטר ב/R/project/WineQuality-WhiteAndRedWine.csv"
wine_data <- read.csv(file_path)
library(tidyverse)
#install.packages("wine_data")
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
body_score <- wine_data$alcohol * wine_data$density
###measure 8
impact_score <- wine_data$alcohol + 
  wine_data$sulphates - 
  wine_data$volatile.acidity
###measure 9
quality_potential <- wine_data$alcohol + 
  wine_data$sulphates + 
  wine_data$citric.acid - 
  wine_data$volatile.acidity - 
  wine_data$chlorides
###measure 10
comfort_score <- 
  (wine_data$alcohol * 1.2) +
  (1 / (wine_data$volatile.acidity + 0.01)) +
  (1 / (wine_data$fixed.acidity + 0.01)) -
  (wine_data$density * 1.5)
### measure 11
flow_score <- 
  (1.8 * wine_data$alcohol) +
  (1.2 / (wine_data$volatile.acidity + 0.01)) +
  (0.8 * wine_data$sulphates) -
  (1.5 * wine_data$density)
### measure 12
silky_score <- 
  (1.5 * wine_data$alcohol) +
  (1 / (wine_data$volatile.acidity + 0.01)) +
  (1 / (wine_data$fixed.acidity + 0.01)) +
  (0.5 * wine_data$citric.acid)
### measure 13
smooth_power_score <- 
  (2 * wine_data$alcohol) +
  (1 / (wine_data$volatile.acidity + 0.01)) +
  (0.5 * wine_data$sulphates) -
  (1.5 * wine_data$density)
### measure 14
zen_balance_score <- 
  (1.5 * wine_data$alcohol) +
  (1.5 * wine_data$citric.acid) +
  (1 / (wine_data$volatile.acidity + 0.01)) +
  wine_data$pH -
  (1.2 * wine_data$density) -
  wine_data$chlorides
###measure 15
easy_drink_score <- 
  (2 * wine_data$alcohol) + 
  (1 / (wine_data$volatile.acidity + 0.01)) + 
  (0.8 * wine_data$sulphates) - 
  (1.5 * wine_data$density) - 
  (1 * wine_data$chlorides)
###mesure 16
next_level_score <- 
  (2 * wine_data$alcohol) + 
  (1 / (wine_data$volatile.acidity + 0.01)) + 
  (0.8 * wine_data$sulphates) +
  (0.5 * wine_data$citric.acid) -
  (1.5 * wine_data$density) -
  (1 * wine_data$chlorides)