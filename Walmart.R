# Useful libraries for this analysis
library(vroom)
library(recipes)
library(tidyverse)
library(tidymodels)
library(ggmosaic)
library(skimr)
library(dplyr)
library(DataExplorer)
library(corrplot)
library(embed)
library(discrim)
library(kernlab)
library(themis)
library(ggplot2)
library(GGally)

# Read in test and training data
train <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/train.csv.zip") |>
  mutate(Date = mdy(Date))
test <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/test.csv.zip")
stores <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/stores.csv")
features <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/features.csv.zip") |>
  mutate(Date = mdy(Date))

# Cleaning and joining
big <- train %>%
  left_join(features, by = c("Store", "Date", "IsHoliday")) %>%
  left_join(stores,   by = "Store")

#executive decision to say NA is 0 for markdowns
big <- big %>%
  mutate(across(starts_with("MarkDown"), ~replace_na(.x, 0)))


onehot <- model.matrix(~ Type - 1, data = big)
big1 <- cbind(big, onehot) %>% 
  select(-Type)

big1$IsHoliday <- as.numeric(big1$IsHoliday)

# EDA
skim(walmartTrain)
glimpse(walmartTrain)
plot_histogram(walmartTrain)
plot_correlation(walmartTrain)
summary(walmartTrain)

# Data types present an issue, IsHoliday is a True/False we should encode as 0/1 so it is useful
# Also maybe date is bad or should be filtered

# Identifying outliers
ggplot(big, aes(x = Weekly_Sales)) +
  geom_histogram(bins = 5, fill = "skyblue", color = "white") +
  labs(title = "Weekly Sales Distribution")

ggplot(big, aes(x = IsHoliday, y = Weekly_Sales, fill = IsHoliday)) +
  geom_boxplot() +
  labs(title = "Weekly Sales: Holiday vs Non-Holiday")

ggplot(big, aes(x = Type, y = Weekly_Sales, fill = Type)) +
  geom_boxplot() +
  labs(title = "Weekly Sales by Store Type")

