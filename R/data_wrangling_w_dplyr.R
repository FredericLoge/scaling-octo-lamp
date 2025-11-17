# DESCRIPTION ------------------------------------------------------------------
#
# In this document we investigate only a fraction of the large panel of methods
# provided by dplyr. The R Documentation & cheatsheet are invaluable resources. 
#
# dplyr cheatsheet: 
#   https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf
#
#

# load library
library(dplyr)

# prepare dataset
data(starwars)

# nicer 'str'
glimpse(starwars)

# select columns
starwars |> select(name, height, mass) # by actual column name
starwars |> select(name:mass)          # all columns from name to mass
starwars |> select(starts_with("s"))   # all columns starting with "s"

# extract some rows based on index, at random or min/max over column
starwars |> slice(3:10) 
starwars |> slice_sample(n=5)
starwars |> slice_max(order_by=birth_year,n=1)

# filter rows on condition (SQL: WHERE)
starwars |> filter(name=='Luke Skywalker')
starwars |> filter(grepl('Skywalker', name)) # boolean vector test rules apply
starwars |> filter(grepl('Skywalker', name), sex=='female') 
starwars |> filter(grepl('Skywalker', name) & sex=='female') 

# mutate new or existing columns (SQL: done in SELECT)
starwars |> mutate(age_20_by=birth_year-20)
starwars |> separate(col=name, into=c('firstname', 'lastname'), sep=' ', remove=FALSE, extra="merge")

# arrange rows (SQL: ORDER BY)
starwars |> arrange(birth_year)

# pivot data into wider or longer format
# > quite commonly used jointly with ggplot
starwars |> 
  select(name, height, mass) |> 
  pivot_longer(cols=-name, names_to='variable_name', values_to='variable_value')

# groupby - summarise
starwars |> count(hair_color) # shortcut
starwars |> 
  group_by(hair_color) |>
  summarise(
    n_characters=n(), 
    n_distinct_eye_color=n_distinct(eye_color), 
    mean_height=mean(height, na.rm=TRUE)
  )

# join example
starwars |>
  select(name, hair_color) |>
  left_join(
    x=_, 
    y=starwars |> select(name, eye_color)
  )

