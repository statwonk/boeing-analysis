library(tidyverse)
library(mdbr)
# source: https://data.ntsb.gov/avdata

# list tables
mdb_tables("avall.mdb")
# read events data
read_mdb("avall.mdb", "events") -> events
read_mdb("avall.mdb", "aircraft") -> aircraft

events %>%
    mutate(date = substr(ev_id, 1, 8),
           date = lubridate::parse_date_time2(date, "Ymd")) %>%
    left_join(aircraft, by = "ev_id") %>%
    distinct(ev_id, Aircraft_Key, .keep_all = T) -> d

# Count events by make and quarter
d %>%
    count(acft_make,
          accident_date = lubridate::floor_date(date, "half")) %>%
    # Fill in quarters with 0 to prevent common mistakes that can afflict analysis
    group_by(acft_make) %>%
    complete(accident_date = seq.POSIXt(min(accident_date), max(accident_date), by = "6 month"), 
             fill = list(n = 0)) %>%
    mutate(n_minus_avg = n - mean(n)) %>%
    ungroup() -> events_by_make2

events_by_make2 %>%
    filter(acft_make %in% c("BOEING", "AIRBUS")) %>%
    # remove partial current half
    filter(accident_date < max(accident_date)) %>%
    ggplot(aes(accident_date, n, color = acft_make)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0) +
    ggtitle("NTSB events per quarter") +
    ylab("NTSB events per quarter") +
    xlab("Event Date") +
    ggthemes::scale_color_colorblind() +
    theme_bw(17) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "top",
          axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
          panel.grid = element_blank())


# Let's adjust for the number of departures performed
# https://www.transtats.bts.gov/databases.asp?Z1qr_VQ=E&Z1qr_Qr5p=N8vn6v10&f7owrp6_VQF=D
list.files(pattern = ".csv") %>%
    map_df(~ read_csv(.x) %>% mutate(file = .x)) %>%
    mutate(year = substr(file, 1, 4)) %>%
    janitor::clean_names() %>%
    select(year, month, aircraft_type, passengers, departures_performed, seats) %>%
    mutate(date = as.POSIXct(paste(year, sprintf("%02d", month), "01", sep = "-"), 
                             format = "%Y-%m-%d",
                             tz = "UTC")) %>%
    group_by(date = lubridate::floor_date(date, "quarter"),
             aircraft_type) %>%
    left_join(
        read_csv("L_AIRCRAFT_TYPE.csv",
                 show_col_types = FALSE) %>%
            janitor::clean_names() %>%
            rename(aircraft_type = code),
        by = "aircraft_type") %>%
    mutate(type = case_when(grepl("Boeing", description, ignore.case = T) ~ "Boeing",
                            grepl("Airbus", description, ignore.case = T) ~ "Airbus",
                            TRUE ~ "All other makers")) %>%
    group_by(date, type) %>%
    summarise(departures_performed = sum(departures_performed),
              passengers_flown = sum(passengers),
              seats_flown = sum(seats),
              .groups = "drop") %>%
    arrange(type, date) %>%
    ungroup() -> flights_per_quarter


events_by_make2 %>%
    mutate(type = case_when(grepl("Boeing", acft_make, ignore.case = T) ~ "Boeing",
                            grepl("Airbus", acft_make, ignore.case = T) ~ "Airbus",
                            TRUE ~ "All other makers")) %>%
    filter(type != "All other makers") %>%
    select(type, date = accident_date, n) %>%
    group_by(type, date) %>%
    summarise(events = sum(n), .groups = "drop") %>%
    inner_join(flights_per_quarter) %>%
    filter(date < max(date)) -> events_by_make3


# Assume 0 events given 0 departures by not including an intercept
lm(events ~ I(departures_performed / 1e5) : type - 1, 
   data = events_by_make3) -> fit

summary(fit)

events_by_make3 %>%
    mutate(events_per_100k_departures = events / (departures_performed / 1e5)) %>%
    ggplot(aes(date, events_per_100k_departures, color = factor(type))) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0) +
    ggthemes::scale_color_colorblind(name = "",
                                     guide = guide_legend(override.aes = list(linewidth = 5))) +
    theme_bw(17) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "top",
          axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
          panel.grid = element_blank()) +
    ylab("NTSB events per 100k depatures") +
    ggtitle("NTSB events per 100k depatures") +
    scale_y_continuous(breaks = seq(0, 10, 2)) +
    xlab("Year")

