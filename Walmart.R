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
walmartTrain <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/train.csv.zip")
walmartTest <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/test.csv.zip")

# EDA
skim(walmartTrain)
glimpse(walmartTrain)
plot_histogram(walmartTrain)
plot_correlation(walmartTrain)
summary(walmartTrain)

# Data types present an issue, IsHoliday is a True/False we should encode as 0/1 so it is useful
# Also maybe date is bad or should be filtered

# Identifying outliers
ggplot(df, aes(x = factor(Store), y = Weekly_Sales)) +
  geom_boxplot()



