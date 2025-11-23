# EXERCISE
#
# Generate IDs under the following rules
# - an ID consists of 10 digits (0-9) chosen at random, 
#   a special character (@, é, è, ê) and 
#   5 letters (lowercase or uppercase no matter)
# - to be valid, the sum of the digits must be lower than 70 (rule 1)
# - to be valid, you cannot have the letter e in the ID and a 0 or 9 
#   in the digits (rule 2)

# libraries & methods ----------------------------------------------------------
# each method with proposed improvements over the last

library(microbenchmark)

gen_id_0 <- function(n){
  # iterate until conditions are met
  vec_id <- rep(NA, n)
  i = 0
  while(i <= n){
    p1 <- runif(n=10,min=0, max=10) |> round() 
    p2 <- sample(x=c('@', 'é', 'è', 'ê'), size=1)
    p3 <- sample(x=c(letters, LETTERS), size=5)
    rule1 <- (sum(p1) < 70)
    rule2 <- !(any(c(0,9) %in% p1) & any(c('e', 'E') %in% p3))
    if(rule1 & rule2){
      vec_id[i] <- paste0(c(p1, p2, p3), collapse='')
      i = i+1
    }
  }
  return(vec_id)
}

sub_gen_id_1 <- function(n){
  # generate a lot of samples and keep only the working ones
  p1 <- runif(n=10*n,min=0, max=10) |> round() |> matrix(data=_, ncol=10) 
  p2 <- sample(x=c('@', 'é', 'è', 'ê'), size=n, replace=TRUE)
  p3 <- sample(x=c(letters, LETTERS), size=5*n, replace=TRUE) |> matrix(data=_, ncol=5)
  rule1 <- rowSums(p1) < 70
  rule2 <- !((rowSums(p1==0 | p1==9)>0) & (rowSums(p3=='e' | p3=='E')>0))
  tbl <- cbind(p1, p2, p3)[rule1 & rule2,] |> as.data.frame()
  ids <- do.call(paste0, c(tbl, sep=''))
  return(ids)
}

gen_id_1 <- function(n){
  # iterate the data generation until we have the right number
  vec_id <- rep(NA, n)
  idx <- 0
  while(idx < n){
    n_missing <- (n-idx)
    print(n_missing)
    new_ids <- sub_gen_id_1(n=n_missing) # you can adapt this to make it speedier
    if(length(new_ids) > n_missing){
      new_ids <- new_ids[1:n_missing]
    }
    vec_id[(idx+1):(idx+length(new_ids))] <- new_ids
    idx <- idx+length(new_ids)
  }
  return(vec_id)
}

# test -------------------------------------------------------------------------

n <- 1e4

microbenchmark(
  gen_id_0(n=n), 
  gen_id_1(n=n), 
  # vectorization is a great tool to speed up computations
  # if you have some constraints, you can still do loops, just much less and more efficiently
  times=5
)

