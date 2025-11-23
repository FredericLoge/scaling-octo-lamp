# for onyxia
# in terminal, start master and worker:
#
# $SPARK_HOME/sbin/start-master.sh
# $SPARK_HOME/sbin/start-worker.sh spark://$(hostname):7077

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(sparklyr)
library(arrow)

# set SPARK_MASTER url
Sys.setenv(SPARK_MASTER = paste0("spark://", Sys.info()[["nodename"]], ":7077"))

# setup connection
sc <- spark_connect(master = Sys.getenv("SPARK_MASTER"))

# check spark version used
spark_version(sc)

# check: no installed version
# sparklyr::spark_installed_versions()

# copy data: create a Spark table flights
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")

# NOTE
#
# ´copy_to()´ moves your data from R to Spark
# ´collect()´ moves your data from Spark to R

# show tables
src_tbls(sc)

# 
flights_tbl

# Link to the track_metadata table in Spark
flights_tbl <- tbl(sc, "flights")

# which class it belongs to
class(flights_tbl)

# computation 1
flight_delay <- 
  flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), 
            dist = mean(distance, na.rm = TRUE), 
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  mutate(delay_by_distance = delay / dist) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  arrange(desc(delay_by_distance))

model_delay <- flights_tbl %>%
  na.omit(arr_delay, distance) %>%
  ml_linear_regression(x=., formula=arr_delay~distance)

# computation 2
flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(median_dist = median(distance))

# graph from computation 1
# system.time(
ggplot(flight_delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)
# )

collected_flight_delay <- flight_delay %>%
  collect()

class(flight_delay)
class(collected_flight_delay)

model_delay_2 <- lm(data=collected_flight_delay, formula=delay~dist)

# graph from computation 1
system.time(
  ggplot(collected_flight_delay, aes(dist, delay)) +
    geom_point(aes(size = count), alpha = 1/2) +
    geom_smooth() +
    scale_size_area(max_size = 2)
)

library(DBI)
query = "select month, day, count(*) as count 
from flights
group by month, day
having count > 365
order by -count"

# evaluate the query and move all the results to R
dbGetQuery(sc, query) %>% head(10)

sdf_len(sc, 5, repartition = 1) %>%
  spark_apply(function(e) e*2)

sdf_len(sc, 5, repartition = 1) %>%
  mutate(id=id*2)

# to go further:
# https://spark.posit.co/guides/distributed-r.html
