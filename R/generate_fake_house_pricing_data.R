generate_fake_house_pricing_data <- function(n_observations, csv_filename){
  
  # geographical position
  lat <- runif(n=n_observations, min=0, max=1)
  long <- runif(n=n_observations, min=0, max=1)
  eps <- 0.05
  close_to_border <- (lat < eps) | (lat > 1-eps) | (long < eps) | (long > 1-eps)
  close_to_center <- (abs(lat-1/2)<eps) & (abs(long-1/2)<eps)

  # house indicator + # of m2  
  house <- sample(x=c(1, 0), size=n_observations, replace=TRUE, prob=c(0.15, 0.85))
  mean_m2 <- rep(60, n_observations) ; mean_m2[house==1] <- 120
  std_m2 <- rep(5, n_observations) ; std_m2[house==1] <- 10
  m2 <- exp(rnorm(n=n_observations, mean=log(mean_m2), sd=log(std_m2)))
  m2 <- pmax(9, m2)
  
  # compute price
  price_mean <- 1e5 + 1e5*m2 + 1e5*house + 1e5*close_to_border + 2e5*close_to_center
  price <- rnorm(n=n_observations, mean=price_mean, sd=price_mean/1e7)
  
  # compile data and save locally
  house_dataset <- cbind(price, lat, long, house, m2)
  write.csv(x=house_dataset, file=csv_filename)
  
}

generate_fake_house_pricing_data(n_observations=1e8, csv_filename='data/house_price_large.csv')

# not working - data is ~7.5Gb
house_price_large <- read.csv('data/house_price_large.csv')


house_price_large_subset <- read.csv('data/house_price_large.csv', nrows=1e4)
str(house_price_large_subset)

hist(log(house_price_large_subset$price))
hist(house_price_large_subset$lat)
hist(house_price_large_subset$long)

# ...
lm(
  formula=price~m2+house+long+lat,
  data=house_price_large_subset
)

n_rows_total <- 1e8
n_rows_per_iter <- 1e6
n_iter <- ceiling(n_rows_total/n_rows_per_iter)
n_rows_read <- 0

x_col_idx <- c(1, 3:6)
y_col_idx <- 2
x_col_len <- length(x_col_idx)

xtx = matrix(data=0, nrow=x_col_len, ncol=x_col_len)
xty = matrix(data=0, nrow=x_col_len, ncol=1)
beta_cum = matrix(data=NA, nrow=x_col_len, ncol=n_iter)
error = matrix(data=NA, nrow=3, ncol=n_iter)

library(progress)
library(readr)
library(data.table)
# data.table::update_dev_pkg()
read_data_method <- 'fread' # one of ('read_csv', 'fread', 'read.csv')
pb <- progress_bar$new(
  format = "  computing [:bar] :percent eta: :eta",
  total = n_iter, clear = FALSE, width= 60)
# con = file('data/house_price_large.csv', open='rb')
for(iteration in 1:n_iter){
  
  # read part of the data
  if(read_data_method=='read.csv'){
    house_price_large_iter <- (
      read.csv(
        file = 'data/house_price_large.csv', 
        nrows=n_rows_per_iter, 
        skip=1+n_rows_read, 
        header=FALSE
      )
    )
  }else if(read_data_method=='read_csv'){
    house_price_large_iter <- (
      read_csv(
        file = con,
        n_max=n_rows_per_iter,
        skip=1+n_rows_read, 
        col_names=FALSE, 
        show_col_types = FALSE
      )
    )
  }else if(read_data_method=='fread'){
    house_price_large_iter <- fread(
      file='data/house_price_large.csv', 
      sep=',', 
      nrows=n_rows_per_iter, 
      header=FALSE, 
      skip=1+n_rows_read
    )
  }else{
    stop(paste0("Read data method ", read_data_method, " not supported."))
  }
  
  # convert data as matrix
  house_price_large_iter <- as.matrix(house_price_large_iter)
  house_price_large_iter[,1] <- 1 # representing the intercept
  
  # compute (X^T * X) and (X^T Y) for this data subset
  xtx_iter = t(house_price_large_iter[,x_col_idx]) %*% house_price_large_iter[,x_col_idx]
  xty_iter = t(house_price_large_iter[,x_col_idx]) %*% house_price_large_iter[,y_col_idx]

  # add both matrices to the global matrices
  xtx = xtx + xtx_iter
  xty = xty + xty_iter
  
  # look at error on next data
  if(iteration >= 2){
    pred_iter = t(house_price_large_iter[,x_col_idx]) %*% beta_cum[,iteration-1]
    err_iter = house_price_large_iter[,y_col_idx] - pred_iter
    error[,iteration] = c(
      mean(err_iter),
      mean(abs(err_iter)),
      sqrt(mean(err_iter**2))
    )
  }
  
  # estimate beta from those matrices
  beta_cum[,iteration] = solve(xtx) %*% xty
  
  # update nb of rows read
  n_rows_read <- n_rows_read + n_rows_per_iter
  
  # update progress bar
  pb$tick()
  
}
close(con)

# ...
matplot(t(beta_cum), type='l')
plot.ts(beta_cum[1,])
plot.ts(beta_cum[2,])

# strategies : read part of the file (but shuffle it)
# create a connection to the file ; read rows based on the connection ; move on
