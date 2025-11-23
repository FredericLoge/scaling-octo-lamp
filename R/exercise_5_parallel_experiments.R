# EXERCISE
#
# Objective is to exploit fully the hardware we have at our disposal
# The future package provides what we need.
# Task; define a slow function and apply the dofuture operator to speed it up

library(future)
library(doFuture)

# setup plan
plan(multisession, workers=4)

# choose a slow function
slow_fct <- function(n_iter=1000, x0=0.5, lambda=3){
  for(i in 1:n_iter){
    x0 <- lambda * x0 * (1-x0)
  }
  return(x0)
}

# choose a fast function
fast_fct <- function(value=0){
  return(value)
}

# main vector
x <- runif(n=500, min=0, max=4)

# compare runtime of slow function -------------------------------------------

# baseline
system.time({
  sapply(X=1:length(x), FUN=function(i){ 
    slow_fct(n_iter = 100000, x0 = 0.5, lambda=x[i])
  })
})

# iterate through the list
system.time({
  iter <- 1:length(x)
  foreach(i=iter, .combine='c') %dofuture% {
    slow_fct(n_iter = 100000, x0 = 0.5, lambda=x[i])
  }
})

# compare runtime of fast function -------------------------------------------

# baseline
system.time({
  sapply(X=1:length(x), FUN=function(i){ 
    fast_fct(value=x[i])
  })
})

# iterate through the list
system.time({
  iter <- 1:length(x)
  foreach(i=iter, .combine='c') %dofuture% {
    fast_fct(value=x[i])
  }
})


