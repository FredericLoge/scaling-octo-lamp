# DBI connection
library(dplyr)

# setup SQLlite db
con <- DBI::dbConnect(RSQLite::SQLite(), filename = ":memory:")

# add connection of the db
mtcars_db <- dplyr::copy_to(con, mtcars)

# perform operations using dplyr verbs
mtcars_db %>%
  filter(cyl > 2) %>%
  select(mpg:hp) %>%
  head(10) %>%
  show_query()
#> <SQL>
#> SELECT `mpg`, `cyl`, `disp`, `hp`
#> FROM `mtcars`
#> WHERE (`cyl` > 2.0)
#> LIMIT 10

# perform operations
mtcars_db %>%
filter(cyl > 2) %>%
  summarise(m=mean(mpg))

# don't forget to close the connection  
DBI::dbDisconnect(con)
