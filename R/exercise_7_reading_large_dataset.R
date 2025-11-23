# EXERCISE 
#
# Reading data in R, and data subsets 

# libraries 

library(readr)
library(data.table)

# reading first rows -> data.table very efficient ----------------------------

fp <- 'data/house_price_large.csv'

nr <- 1e6

# base
read.csv(file=fp, nrows=nr) |> system.time()

# readr (better than base)
read_csv(file=fp, n_max=nr, show_col_types=F) |> system.time()

# data.table (better than above)
fread(file=fp, nrows=nr) |> system.time()

# reading subset of rows  ----------------------------------------------------

# capture nb of rows
n_rows <- system('wc -l data/house_price_large.csv', intern = TRUE)
n_rows <- as.integer(strsplit(x=n_rows, split=' ')[[1]][2]) - 1

# capture random subset
# this is soooo long
data_subset <- sapply(X=as.integer(round(runif(n=10, min=1, max=n_rows))), FUN=function(j){
  fread(file=fp, nrows=1, skip=j)
})
