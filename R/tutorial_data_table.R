library(data.table)

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
longDT <- melt(DT, id.vars = c("id", "year", "category"])

print(longDT)

# long → wide
wideDT <- dcast(longDT, id + category ~ variable, fun = mean)
print(wideDT)

# fread("data.csv")  # fast file import (not run here)
