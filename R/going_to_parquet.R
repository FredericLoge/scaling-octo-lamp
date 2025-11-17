# lien pour SAS:

library(parquetize)
# install.packages("arrow", type = "binary")
# remotes::install_github("ddotta/parquetize") # takes some time


# https://www.futureverse.org/
fp <- 'https://drive.google.com/file/d/1-c7PkjbuT9K502uKu34nYFQJQTpvVJsa/view?usp=drive_link'
csv_to_parquet(path_to_file=fp, path_to_parquet='data/local_parquet.parquet')

csv_to_parquet(path_to_file='data/house_price_large.csv', path_to_parquet='data/local_parquet.parquet')

  
plan(multisession)  # or some other plan

library(doFuture, quietly = TRUE)
registerDoFuture()

out <- foreach(i = 1:5) %dofuture% {
  cat("Running in process", Sys.getpid(), "\n")
  mean(1:i)
}
