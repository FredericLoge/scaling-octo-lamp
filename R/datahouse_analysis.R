library(dplyr)
houses <- read_csv('data/house_price_large.csv', n_max = 1e6)

mod <- lm(data=houses, formula=price~.)

system.time({
  v <- rep(NA, nrow(houses))
  for(i in 1:nrow(houses)){
    v[i] <- houses$price[i]
    v[i] <- v[i]**2
  }
})

system.time({
  v <- houses$price**2
})

