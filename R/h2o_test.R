# h2o documentation:
# https://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/index.html

library(mlbench)
library(h2o)
# install.packages("h2o")
library(tidymodels)

data("BostonHousing")
new_h2o_data <- BostonHousing
index <- initial_split(new_h2o_data)
train_h2o_data <- training(index)
test_h2o_data <- testing(index)

h2o.init()
train_h2o_data <- as.h2o(train_h2o_data)

auto_ml <- h2o.automl(y='crim', training_frame=train_h2o_data, max_runtime_secs=20)

best_model <- h2o.get_best_model(auto_ml)

test_h2o_data <- as.h2o(test_h2o_data)
prediction <- h2o.predict(best_model, test_h2o_data)

auto_ml

dt_model <- h2o.glm(y='crim', training_frame=train_h2o_data)

library(dplyr)
library(readr)
library(randomForest)

x <- read_csv(file='data/house_price_large.csv', n_max=1e5)
x_h2o <- as.h2o(x)
system.time({
  dt_model <- h2o.randomForest(y='price', training_frame=x_h2o)
})

system.time({
  dt_model <- randomForest(formula=price~., data=x)
})
