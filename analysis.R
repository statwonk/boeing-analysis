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

# Count accidents by make
d %>% 
    count(acft_make, sort = T)

# Count accidents by make and quarter
d %>%
    count(acft_make,
          accident_date = lubridate::floor_date(date, "quarter")) -> accidents_by_make

# Fill in quarters with 0 to prevent common mistakes that can afflict analysis
accidents_by_make %>%
    group_by(acft_make) %>%
    complete(accident_date = seq.POSIXt(min(accident_date), max(accident_date), by = "quarter"), 
             fill = list(n = 0)) %>%
    mutate(n_minus_avg = n - mean(n)) %>%
    ungroup() -> accidents_by_make2

accidents_by_make2 %>%
    filter(acft_make %in% c("BOEING", "AIRBUS")) %>%
    ggplot(aes(accident_date, n, color = acft_make)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0) +
    ggthemes::scale_color_colorblind() +
    geom_smooth(method = "gam",
                formula = y ~ s(x, bs = "ps")) +
    # coord_cartesian(ylim = c(0, 120)) +
    theme_bw() +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "top")
    
    
    
