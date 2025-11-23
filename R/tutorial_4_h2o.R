# h2o documentation:
# https://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/index.html

# load libraries
library(mlbench)
library(h2o)
library(tidymodels)

# prepare data
data("BostonHousing")

# copy data and split in train-test
new_h2o_data <- BostonHousing
index <- initial_split(new_h2o_data)
train_h2o_data <- as.h2o(training(index))
test_h2o_data <- as.h2o(testing(index))

# start h2o local JAVA server
h2o.init()

# run auto ML on training data
auto_ml <- h2o.automl(y='crim', training_frame=train_h2o_data, max_runtime_secs=20)

# extract best model
best_model <- h2o.get_best_model(auto_ml)

# check out predictions
prediction <- h2o.predict(best_model, test_h2o_data)