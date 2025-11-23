# libraries --------------------------------------------------------------------

# install.packages("arrow", type = "binary")
# remotes::install_github("ddotta/parquetize")
library(parquetize)

library(arrow)

library(DBI)
library(duckdb) # <!> takes some time to install

# create parquet from the csv --------------------------------------------------
# you'll notice a substantial data size change

csv_to_parquet(
  path_to_file='data/house_price_large.csv', 
  path_to_parquet='data/local_parquet.parquet'
)

# read parquet using arrow -----------------------------------------------------
# note: you can also read csv with this, and many other formats
# note: the object read is not a data object!

house_parquet <- read_parquet(file="data/local_parquet.parquet", as_data_frame = FALSE)

house_parquet

req <- house_parquet %>% 
  select(price, lat, long) %>% 
  filter(lat > 0.5) %>%
  summarise(mean(price)) # use usual dplyr verbs

print(req)

req |> collect() # actually run the query (on the whole database)

# read parquet using duckdb ----------------------------------------------------

# prepare DB connection
con <- dbConnect(duckdb())

# setup duck db connection 
# you can also read directly the parquet without relying on the previous read_parquet
house_parquet_tbl <- to_duckdb(house_parquet, con, "house_parquet_con")

# same code as before 
# -> this runs the query BUT the result is not stored
req <- house_parquet_tbl %>% 
  select(price, lat, long) %>% 
  filter(lat > 0.5) %>%
  summarise(mean(price))

# run and export the value
res <- req |> collect()
