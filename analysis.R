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
          accident_date = lubridate::floor_date(date, "half")) -> accidents_by_make

# Fill in quarters with 0 to prevent common mistakes that can afflict analysis
accidents_by_make %>%
    group_by(acft_make) %>%
    complete(accident_date = seq.POSIXt(min(accident_date), max(accident_date), by = "6 month"), 
             fill = list(n = 0)) %>%
    mutate(n_minus_avg = n - mean(n)) %>%
    ungroup() -> accidents_by_make2

accidents_by_make2 %>%
    filter(acft_make %in% c("BOEING", "AIRBUS")) %>%
    # remove partial current half
    filter(accident_date < max(accident_date)) %>%
    ggplot(aes(accident_date, n, color = acft_make)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0) +
    ggthemes::scale_color_colorblind() +
    theme_bw(17) +
    ggtitle("Accidents per quarter") +
    ylab("Accidents per quarter") +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "top",
          axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
          panel.grid = element_blank())
    
    
    
