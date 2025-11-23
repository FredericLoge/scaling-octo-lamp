# DESCRIPTION ------------------------------------------------------------------
# 
# In this example we investigate how sample size affects model performance &
# demonstrate that not all samples are required to reach performance plateau.
# On this specific examples 1K samples were necessary out of 12K available.
# Of course, this is linked to the complexity of the data and the modelling,
# 1/12 is not to be taken as a rule of thumb for other cases. However, it tends
# to show that slowly increasing the sample size until we near a performance
# plateau can be a proper technique justifying subsampling on really large 
# datasets. This is somewhat similar to the early stopping approach used 
# in neural networks training (the nnets run through the whole dataset each 
# epoch, but if at some point there is no perf improvement the training stops).

# load necessary libraries
library(survival)
library(pROC)
library(tidyverse)

# prepare dataset --------------------------------------------------------------

# copy dataset Non-alcoholic fatty liver disease and remove NA
nafld1_personal <- nafld1 |> drop_na()

# select 1K samples (out of 12K) for testing purposes
test_ids <- sample(x=nafld1_personal$id, size=1000, replace=FALSE)

# split dataset in training and testing
nafld1_train <- nafld1_personal[-test_ids,]
nafld1_test <- nafld1_personal[test_ids,]
rm(nafld1_personal)

# run experiment ---------------------------------------------------------------

# result storage
result <- list()

# iterate on nb of samples acquired
for(n_samples in c(seq(from=100, to=3000, by=100), Inf)){
  
  # repeat operation many times
  n_iter <- ifelse(is.infinite(n_samples), 1, 10)
  for(iter in 1:n_iter){
    
    # subsample at random
    if(is.infinite(n_samples)){
      nafld1_train_subsample <- nafld1_train      
    }else{
      nafld1_train_subsample <- nafld1_train %>% slice_sample(n=n_samples, replace=FALSE)
    }
    
    # train logistic regression
    mod <- glm(formula=status~., data=nafld1_train_subsample, family=binomial())
    
    # record training AUC
    pred_train <- predict(mod, newdata=nafld1_train_subsample, type='response')
    auc_train <- roc(mod$data$status, pred_train) |> auc() |> as.numeric()

    # record testing AUC
    pred_test <- predict(mod, newdata=nafld1_test, type='response')
    auc_test <- roc(nafld1_test$status, pred_test) |> auc() |> as.numeric()

    # output record
    result[[length(result)+1]] <- c(n_samples, iter, auc_train, auc_test)
    
  }
}  

# compile results and format in tibble
result_compiled <- result |>
  do.call(what=rbind) |>
  as_tibble() |>
  setNames(c('n_samples', 'idx', 'auc_train', 'auc_test')) |>
  pivot_longer(cols = c('auc_train', 'auc_test'))

# present result ---------------------------------------------------------------

# extract the auc_test performance assuming we used 100% of the training data available
auc_test_full_dataset <- (
  result_compiled |> 
  filter(is.infinite(n_samples), name=='auc_test') |>
  pull(value)
)

# graph performance per % of the available data in training
ggplot(data=result_compiled)+
  aes(x=n_samples, y=value, col=name |> factor(levels=c('auc_train', 'auc_test'), labels=c('Train (varying #)', 'Test (fixed 1K)')))+
  geom_hline(yintercept=auc_test_full_dataset, color='purple', lwd=3)+
  geom_point()+
  geom_smooth()+
  labs(y='Area Under the Curve', 
       x='# of samples in train set (subset of 12K train set)',
       color='Dataset',
       title='Non-alcoholic fatty liver disease prediction AUC',
       subtitle='train set (blue), test set (red) and full train dataset (purple)',
       caption='The full train dataset is about 12K individuals, and the test set is 1K individuals.')+
  scale_y_continuous(labels = scales::percent_format(), limits=c(0.5,1))+
  theme_light()+
  theme(text=element_text(size=15), legend.position='top')