# Useful libraries for this analysis
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)
library(dplyr)
library(lubridate)
library(prophet)
library(patchwork)

# Read in test and training data
train <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/train.csv") |>
  mutate(Date = ymd(Date))
test <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/test.csv") |>
  mutate(Date = ymd(Date))
stores <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/stores.csv")
features <- vroom("~/Documents/STAT 348/WalmartForecasting/walmart-recruiting-store-sales-forecasting/features.csv") |>
  mutate(Date = ymd(Date))

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))


# 1. Replace NAs in the Markdown variables with 0
features <- features |>
  mutate(across(starts_with("MarkDown"), ~replace_na(.x, 0)))

# 2. Create a TotalMarkdown variable
#features <- features |>
  #mutate(TotalMarkdown = MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5) |>
  #select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

# 3. Create a MarkdownFlag variable with 1 if there is a markdown and 0 if there is not
#features <- features |>
  #mutate(MarkdownFlag = if_else(TotalMarkdown > 0, 1, 0))

# Join the now imputed features dataset with the test and train data
full_train <- train |>
  left_join(features, by = c("Store", "Date", "IsHoliday")) |>
  left_join(stores,   by = "Store")

full_test <- test |>
  left_join(features, by = c("Store", "Date", "IsHoliday")) |>
  left_join(stores,   by = "Store")

# Change Holiday variable to what the holiday actually is
holiday_lookup <- tibble(
  Date = ymd(c(
    # Super Bowl
    "2010-02-12","2011-02-11","2012-02-10","2013-02-08",
    # Labor Day
    "2010-09-10","2011-09-09","2012-09-07","2013-09-06",
    # Thanksgiving
    "2010-11-26","2011-11-25","2012-11-23","2013-11-29",
    # Christmas
    "2010-12-31","2011-12-30","2012-12-28","2013-12-27"
  )),
  HolidayName = c(
    rep("Super Bowl", 4),
    rep("Labor Day", 4),
    rep("Thanksgiving", 4),
    rep("Christmas", 4)
  )
)

full_train <- full_train |>
  mutate(Date = ymd(Date)) |>
  left_join(holiday_lookup, by = "Date")

full_train <- full_train |>
  mutate(Holiday = if_else(IsHoliday, HolidayName, "None"))

full_test <- full_test |>
  mutate(Date = ymd(Date)) |>
  left_join(holiday_lookup, by = "Date")

full_test <- full_test |>
  mutate(Holiday = if_else(IsHoliday, HolidayName, "None"))

# Testing each store/department combination
train_subset <- full_train |> 
  filter(Store == 5, Dept == 7)

test_subset <- full_test |> 
  filter(Store == 5, Dept == 7)

# ML Recipe
walmart_recipe <- recipe(Weekly_Sales ~., data = train_subset) |>
  step_date(Date, features = "doy") |>
  step_range(Date_doy, min = 0, max = pi) |>
  step_mutate(
    sinDOY = sin(Date_doy),
    cosDOY = cos(Date_doy))


# Random Forest
random_forest_mod <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500
) |>
  set_engine("ranger") |>
  set_mode("regression")

random_forest_wf <- workflow() |>
  add_recipe(walmart_recipe) |>
  add_model(random_forest_mod)

random_forest_grid <- grid_regular(
  mtry(range = c(1, 10)),    # SAFE RANGE (adjust if needed)
  min_n(),
  levels = 5)

folds <- vfold_cv(train_subset, v = 5)

random_forest_CV_results <- random_forest_wf |>
  tune_grid(
    resamples = folds,
    grid = random_forest_grid,
    metrics = metric_set(rmse)   # regression metric
  )

random_forest_bestTune <- random_forest_CV_results |>
  select_best(metric = "rmse")

random_forest_final_wf <- random_forest_wf |>
  finalize_workflow(random_forest_bestTune) |>
  fit(data = train_subset)

random_forest_preds <- test_subset |>
  select(Store, Dept, Date) |>    # keep ID columns
  bind_cols(
    predict(random_forest_final_wf, new_data = test_subset)  # adds .pred
  )

submission <- random_forest_preds |>
  mutate(
    Id = paste(Store, Dept, Date, sep = "_"),
    Weekly_Sales = .pred
  ) |>
  select(Id, Weekly_Sales)


vroom_write(
  submission,
  file = "~/Documents/STAT 348/WalmartForecasting/random_forest.csv",
  delim = ","
)

library(dplyr)
library(tidymodels)

# Get the best CV RMSE as a single number
cv_result <- random_forest_CV_results %>%
  show_best(metric = "rmse", n = 1) %>%  # note the named argument
  pull(mean)  # extract the mean RMSE

cv_result


#### Fit Prophet Model

## Prophet in R
## Need to have the column names right for prophet
prophet_df <- storeDeptData |>
  rename(y=ResponseVar, ds=Date_var)

## Fit prophet Model with extra regressors
prophet_model <- prophet() |>
  add_regressor('x1') |>
  add_regressor('x2') |>
  fit.prophet(prophet_df)

## Predict using Prophet Model
predict(prophet_model, df=new_data)




## Choose Store and Dept
store <- 13
dept <- 13

## Filter and Rename to match prophet syntax
sd_train <- full_train |>
  filter(Store==store, Dept==dept) |>
  rename(y=Weekly_Sales, ds=Date)
sd_test <- full_test |>
  filter(Store==store, Dept==dept) |>
  rename(ds=Date)

## Fit a prophet model
prophet_model <- prophet() |>
  add_regressor("MarkDown_Total") |>
  add_regressor("IsHoliday") |>
  add_regressor("Temperature") |>
  fit.prophet(df=sd_train)

## Predict Using Fitted prophet Model
fitted_vals <- predict(prophet_model, df=sd_train) # For Plotting Fitted Values
test_preds <- predict(prophet_model, df=sd_test) # Predictions are called "yhat"

## Plot Fitted and Forecast on Same Plot
store13dept13 <- ggplot() +
  geom_line(data = sd_train,
            aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals,
            aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds,
            aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black",
                                "Fitted" = "blue",
                                "Forecast" = "red")) +
  labs(color = "")

store20dept20 <- ggplot() +
  geom_line(data = sd_train,
            aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals,
            aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds,
            aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black",
                                "Fitted" = "blue",
                                "Forecast" = "red")) +
  labs(color = "")

store13dept13 + store20dept20
