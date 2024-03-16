library(tidyverse)
library(mdbr)
# source: https://data.ntsb.gov/avdata

# list tables
mdb_tables("avall.mdb")
# read events data
read_mdb("avall.mdb", "dt_events") -> events
read_mdb("avall.mdb", "dt_aircraft") -> dt_aircraft
read_mdb("avall.mdb", "aircraft") -> aircraft


events %>%
    mutate(date = substr(ev_id, 1, 8),
           date = lubridate::parse_date_time2(date, "Ymd")) %>%
    left_join(aircraft, by = "ev_id") -> d

d %>%
    count(date = lubridate::floor_date(date, "month")) %>%
    ggplot(aes(date, n)) +
    geom_bar(stat = "identity")


aircraft
