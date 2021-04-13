# Loading packages and setting seed
library(tidymodels)
library(glmnet)
library(tidyverse)
library(magrittr)
# 4. Reading in the housing data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's

# 5. Setting the seed
set.seed(123456)

# 6. Creating train and test data sets
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# 7. Creating the recipe
housing_recipe <- recipe (medv ~ ., data = housing ) %>%
  step_log ( all_outcomes ()) %>%
  step_bin2factor (chas) %>%
  step_interact ( terms = ~ crim:zn:indus:rm:age:rad:tax:
                     ptratio:b:lstat:dis:nox) %>%
  step_poly (crim ,zn ,indus ,rm ,age ,rad ,tax ,ptratio ,b,
             lstat ,dis ,nox , degree =6)

# Run the recipe
housing_prep <- housing_recipe %>% prep( housing_train , retain
                                             = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select (-medv)
housing_test_x <- housing_test_prepped %>% select (-medv)
housing_train_y <- housing_train_prepped %>% select ( medv)
housing_test_y <- housing_test_prepped %>% select ( medv)
dim(housing_train_x)

# 8. Lasso 
tune_spec <- linear_reg(
  penalty = tune(), 
  mixture = 1       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
lambda_grid <- grid_regular(penalty(), levels = 50)
rec_folds <- vfold_cv(housing_train_x %>% bind_cols(tibble(medv = housing_train_y)), v = 6)
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(medv~ . )
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# 9. 
tune_spec <- linear_reg(
  penalty = tune(), 
  mixture = 0       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
lambda_grid <- grid_regular(penalty(), levels = 50)
rec_folds <- vfold_cv(housing_train_x %>% bind_cols(tibble(medv = housing_train_y)), v = 6)
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(medv~ . )
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")