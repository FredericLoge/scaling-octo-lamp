# EXERCISE
# 
# Implement an efficient moving average computation
# input: numerical vector
# output: moving average y, 
#   where y_i = 1/3*(x_{i-1} + x_{i} + x_{i+1}), NA on the extremities

# libraries & methods ----------------------------------------------------------
# each method with proposed improvements over the last

library(microbenchmark)

ma_0 <- function(x, n){
  # start empty vector
  y <- c(NA) 
  for(i in (2:(n-1))){
    # add new value to a vector
    y <- c(y, (x[i-1]+x[i]+x[i+1])/3)
  }
  return(y)
}

ma_1 <- function(x, n){
  # allocate space for the vector
  y <- rep(NA, n)
  for(i in (2:(n-1))){
    y[i] <- (x[i-1]+x[i]+x[i+1])/3
  }
  return(y)
}

ma_2 <- function(x, n){
  # removing for loop we had in ma_1
  return((c(NA, x[1:(n-1)]) + x + c(x[2:n], NA))/3)
}

# test -------------------------------------------------------------------------

x <- rnorm(n=1e4)
n <- length(x)

microbenchmark(
  ma_0(x=x, n=n), # you should always avoid dynamic memory allocation!
  ma_1(x=x, n=n), # you should avoid (when you can) for loops
  ma_2(x=x, n=n),
  times=5
)
