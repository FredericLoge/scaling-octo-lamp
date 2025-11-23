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
  price_mean <- 1e4 + 1e4*m2 + 1e4*house + 1e4*close_to_border + 2e4*close_to_center
  price <- rnorm(n=n_observations, mean=price_mean, sd=price_mean/1e7)
  
  # compile data and save locally
  house_dataset <- cbind(price, lat, long, house, m2)
  write.csv(x=house_dataset, file=csv_filename)
  
}

generate_fake_house_pricing_data(
  n_observations=1e8, 
  csv_filename='data/house_price_large.csv'
)