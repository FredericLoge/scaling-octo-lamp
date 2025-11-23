# DESCRIPTION ------------------------------------------------------------------
#
# In this document we investigate only a fraction of the large panel of methods
# provided by dplyr. The R Documentation & cheatsheet are invaluable resources. 
#
# dplyr cheatsheet: 
#   https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf
#
#

# load libraries
library(dplyr)
library(tidyr)
library(dtplyr)
library(data.table)

# prepare dataset
data(starwars)

# nicer 'str'
glimpse(starwars)

# select columns
starwars |> select(name, height, mass) # by actual column name
starwars |> select(name:mass)          # all columns from name to mass
starwars |> select(starts_with("s"))   # all columns starting with "s"

# quick note on the pipe operators
# the two pipes are somewhat equivalent but some small differences in behaviour can occur
# for more information, check out https://tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/
starwars |> select(name)
starwars %>% select(name)

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

# dtplyr -----------------------------------------------------------------------

# convert to lazy data.table
lazy_starwars <- lazy_dt(starwars)

# this code is exactly the same as before, but it transforms the code in data.table
# and gives you the actual call in the result, good to ease into data.table syntax
lazy_starwars |> 
  select(name, hair_color) |>
  group_by(hair_color) |>
  summarise(n=n())
  
# data.table -------------------------------------------------------------------

# --- 1) Create a sample data.table ---
DT <- data.table(
  id = rep(1:5, each = 4),
  year = rep(2018:2021, 5),
  value = rnorm(20, mean = 100, sd = 15),
  category = rep(LETTERS[1:2], 10)
)

print(DT)

# --- 2) Fast filtering with i ---
DT[year >= 2020 & category == "B"]

# --- 3) Selecting / aggregating with j ---
DT[, .(mean_value = mean(value)), by = category]

# --- 4) Multiple grouped summaries ---
DT[, .(
  mean_val = mean(value),
  sd_val   = sd(value),
  n        = .N
), by = .(category, year)]

# --- 5) Chaining operations ---
DT[year >= 2019][
  , .(mean_val = mean(value)), by = category
][order(-mean_val)]

# --- 6) Update-by-reference (in-place updates) ---
DT[value < 100, flag := TRUE]
DT[is.na(flag), flag := FALSE]

# Add normalized value by group
DT[, value_z := scale(value), by = category]

print(DT)

# --- 7) Join operations ---
lookup <- data.table(category = c("A", "B"), label = c("Group A", "Group B"))
DT_joined <- lookup[DT, on = .(category)]
print(DT_joined)

# --- 8) Reshaping with melt + dcast ---
# wide → long
longDT <- melt(DT, id.vars = c("id", "year", "category"))

print(longDT)

# long → wide
wideDT <- dcast(longDT, id + category ~ variable, fun = mean)
print(wideDT)

# fread("data.csv")  # fast file import (not run here)


