file_path <- "C:/Users/Nofit/Desktop/רואי/לימודים רואי/שנה ג/סמסטר ב/R/project/WineQuality-WhiteAndRedWine.csv"
wine_data <- read.csv(file_path)
library(tidyverse)
#install.packages("modelr")
#install.packages("wine_data")
library(modelr)
######################Visualizations
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=excellent_good_bad,color=type)) ##ensuring that the equality is enough
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=Good_NotGood,color=type)) ##ensuring that the equality is enough
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=alcohol_level,,color=type)) #ensuring that the equality is enough
#################
ggplot(data = wine_data)+
  geom_histogram(mapping = aes(x=pH,color=type),binwidth = 0.2) ## looks like a normal distribution
ggplot(data = wine_data)+
  geom_histogram(mapping = aes(x=sulphates,color=type)) ## looks like a normal distribution
ggplot(data = wine_data)+
  geom_histogram(mapping = aes(x=fixed.acidity,color=type)) ## looks like a normal distribution
ggplot(data=wine_data)+
  geom_histogram(mapping = aes(x=quality,color=type),binwidth = 1)## looks like a normal distribution, but better only with white wine
ggplot(data=wine_data[wine_data$type=="white",])+
  geom_histogram(mapping = aes(x=quality),binwidth = 1)## better normal distribution
########
ggplot(data = wine_data)+
  geom_histogram(mapping = aes(x=total.sulfur.dioxide,color=type)) ## מדד חריג
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=chlorides,color=type)) ## מדד חריג
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=citric.acid)) ##מדד חריג
ggplot(data = wine_data)+
  geom_bar(mapping = aes(x=volatile.acidity,color=type)) ##מדד חריג
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
  geom_smooth(mapping = aes(x=acidity_score,y=density))  ###in general, more acidity, more density 
cor(acidity_score, wine_data$density) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=acidity_score,y=pH)) ###in general, more acidity less pH
cor(acidity_score, wine_data$pH) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=volatile.acidity,y=quality)) ###in general, more volatile acidity, less quality
cor(acidity_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_line(mapping = aes(x=aroma_score,y=quality)) ###in general, more aroma score, more quality
cor(aroma_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=body_score,y=quality)) ###in general, more body, more quality
cor(body_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=impact_score,y=quality)) ###in general, more impact score, more qualitbalance_scorecore, wine_data$qualtion
cor(impact_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=quality_potential,y=quality)) ###in general, more quality potential, more qualitbalance_scorecore, wine_data$qualtion
cor(quality_potential, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=comfort_score,y=quality)) ###in general, more comfort score, more qualitbalance_scorecore, wine_data$qualtion
cor(comfort_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=silky_score,y=quality)) ###in general, more silky score, more qualitbalance_scorecore, wine_data$qualtion
cor(silky_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=flow_score,y=quality)) ###in general, more flow score, more qualitbalance_scorecore, wine_data$qualtion
cor(flow_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=smooth_power_score,y=quality)) ###in general, more smooth power score, more qualitbalance_scorecore, wine_data$qualtion
cor(smooth_power_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=easy_drink_score,y=quality)) ###in general, more easy drink score, more qualitbalance_scorecore, wine_data$qualtion
cor(easy_drink_score, wine_data$quality) ###checking the correlation
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=next_level_score,y=quality)) ###in general, more next level score, more qualitbalance_scorecore, wine_data$qualtion
cor(next_level_score, wine_data$quality) ###checking the correlation 
ggplot(data=wine_data)+
  geom_count(mapping = aes(x=next_level_score,y=quality))
ggplot(data=wine_data)+
  geom_smooth(mapping = aes(x=perfect_balance_score,y=quality)) ###in general, more next level score, more qualitbalance_scorecore, wine_data$qualtion
cor(perfect_balance_score, wine_data$quality) ###checking the correlation 
ggplot(data=wine_data)+
  geom_count(mapping = aes(x=perfect_balance_score,y=quality))
wine_data %>%
  count(wine_data$next_level_score,quality)
grid <- wine_data %>%
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






