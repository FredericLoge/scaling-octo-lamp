# DBI connection
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), filename = ":memory:")
mtcars_db <- dplyr::copy_to(con, mtcars)

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

mtcars_db %>%
filter(cyl > 2) %>%
  summarise(m=mean(mpg))
  
DBI::dbDisconnect(con)
