library(caret)
library(parallel)
library(foreach)
library(doParallel)
library(itertools)
library(future)
library(doFuture)
library(microbenchmark)

# | Feature                           | doParallel                  | doFuture                                           |
#   | --------------------------------- | --------------------------- | -------------------------------------------------- |
#   | Communication                     | Direct sockets to cluster   | Depends on `plan()` (socket, fork, MPI, batch job) |
#   | Overhead                          | Low                         | Medium–High                                        |
#   | HPC schedulers (Slurm, SGE, etc.) | ❌ No                        | ✔ Yes (via batchtools or future backend)           |
#   | Cloud / remote workers            | Limited                     | Excellent support                                  |
#   | Automatic global export           | ❌ No                        | ✔ Yes                                              |
#   | Nested parallelism                | ❌ Hard                      | ✔ Yes (transparent futures)                        |
#   | Best for                          | Large tasks, simple configs | Complex environments, portability, HPC/cloud       |
  
  
# RECOMMENDATION USE %dofuture% when you have a slow function
# if it's a rapid function, then there's no point!!

# using %dofuture%
plan(multisession)
system.time({
  x <- foreach(i = 1:4) %dofuture% {
    Sys.sleep(2)
  }
})

# select a function to run
slow_fct <- function(x){ log(x) } # not really a slow function
slow_fct <- function(x){ Sys.sleep(time=1e-5); return(log(x)) }

# main vector
x <- c(1:1e6)

# approach A: base
time_base <- system.time({
  sapply(X=1:length(x), FUN=function(i){ slow_fct(x[i]) })
})

# approach B: %dopar%
print(parallel::detectCores())
Ncpus <- 3
cl <- makePSOCKcluster(Ncpus)
registerDoParallel(cl)
time_dopar <- system.time({
  foreach(i=1:length(x), .combine='c') %dopar% {
    slow_fct(x[i])
  }
})

# approach C: %dopar% + itertools
time_dopar_itertools <- system.time({
  iter <- itertools::isplitIndices(n=length(x), chunks = Ncpus)
  foreach(i=iter, .combine='c') %dopar% {
    slow_fct(x[i])
  }
})
stopCluster(cl)

# approach D: %dofuture% multisession
plan(multisession)
time_dofuture <- system.time({
  # iter <- itertools::isplitIndices(n=length(x), chunks = Ncpus)
  iter <- 1:length(x)
  foreach(i=iter, .combine='c') %dofuture% {
    slow_fct(x[i])
  }
})

# comparison
rbind(time_base, time_dopar, time_dopar_itertools, time_dofuture)



training <- read_csv('data/house_price_large.csv', n_max = 1e5)
system.time({
  model <- train(price ~ ., data = training, method = "rf")
})

## When you are done:
stopCluster(cl)

library(foreach)
library(future)
library(doFuture)

xs <- rnorm(1000)
y <- foreach(x = xs) %dofuture% {
  x
}






log_par <- function(x){
  Ncpus <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(Ncpus)
  doParallel::registerDoParallel(cl)
  res <- foreach(i=1:length(x), .combine='c') %dopar% {
    log(x[i])
  }
  parallel::stopCluster(cl)
  return(res)
}

log_seq <- function(x){
  # try this yourself (spoiler alert: it is quite long...):
  # res <- numeric(length(x))
  # for(i in 1:length(x)){
  #   res[i] <- log(x[i])
  # }
  # return(res)
  return(log(x))
}

mb <- microbenchmark(log_par(1:100), log_seq(1:100), times=50)
mb
