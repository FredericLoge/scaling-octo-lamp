# EXERCISE
# 
# Generate different vectors (int, double, character) and look at memory size
# Want to bet on what takes more memory ?
#
# Interesting resource:
#   http://adv-r.had.co.nz/memory.html#:~:text=R%20uses%20an%20alternative%20approach,object%2C%20it%20deletes%20that%20object.

library(lobstr)

# numerical
rnorm(n=1e6) |> obj_size()

# integer (but stored as numerical)
round(rnorm(n=1e6)) |> obj_size()

# integer -> more efficient
round(rnorm(n=1e6)) |> as.integer() |> obj_size()

# character
as.character(rnorm(n=1e6)) |> obj_size()

# categorical
sample(
  x=c('France', 'England', 'Spain', 'Hungary', 'Nigeria', 'Laos', 'Australia'), 
  size=1e6, 
  replace=TRUE
) |> obj_size()

# factor -> more efficient
sample(
  x=c('France', 'England', 'Spain', 'Hungary', 'Nigeria', 'Laos', 'Australia'), 
  size=1e6, 
  replace=TRUE
) |> factor() |> obj_size()

# boolean
(rnorm(n=1e6) > 0) |> obj_size()

# what if you repeat the same sequence ?
banana <- "bananas bananas bananas"
banana |> obj_size()
rep(banana, 100) |> obj_size() # not linear scaling

# what if we create a list from some data and modify some elements of the list ?
v <- rnorm(n=1e6)
toto <- list(v, v, v)
v |> obj_size() 
toto |> obj_size() # same as v (almost)
toto[[1]][1] <- 9
toto |> obj_size() # minimal change: only need two copies

# Conclusion:
# - roughly 8MB/Million
# - can drop if you rely on integer coding (integer, factor, boolean)
# - R has specific internal mechanisms to handle efficiently memory