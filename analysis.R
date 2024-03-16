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
    left_join(aircraft, by = "ev_id") %>%
    distinct(ev_id, Aircraft_Key, .keep_all = T) -> d

d %>%
    distinct(ev_id, Aircraft_Key, .keep_all = T) %>%
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
    xlab("Accident Date") +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.position = "top",
          axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
          panel.grid = element_blank())


# Let's adjust for the number of departures performed
# https://www.transtats.bts.gov/databases.asp?Z1qr_VQ=E&Z1qr_Qr5p=N8vn6v10&f7owrp6_VQF=D
read_csv("2023_T_T100D_SEGMENT_US_CARRIER_ONLY.csv") %>%
    janitor::clean_names() -> flights

flights %>% count(quarter)


read_csv("2022_T_T100D_SEGMENT_US_CARRIER_ONLY.csv") %>%
    janitor::clean_names() -> flights_2022

flights_2022 %>% count(quarter)


list.files(pattern = ".csv") %>%
    map_df(~ read_csv(.x) %>% mutate(file = .x)) %>%
    mutate(year = substr(file, 1, 4)) %>%
    janitor::clean_names() %>%
    select(year, month, aircraft_type, passengers, departures_performed, seats) -> flights


flights %>%
    mutate(date = as.POSIXct(paste(year, month, "01", sep = "-"), tz = "UTC")) %>%
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
              .groups = "drop") %>%
    arrange(type, date) %>%
    ungroup() -> flights_per_quarter


accidents_by_make2 %>%
    mutate(type = case_when(grepl("Boeing", acft_make, ignore.case = T) ~ "Boeing",
                            grepl("Airbus", acft_make, ignore.case = T) ~ "Airbus",
                            TRUE ~ "All other makers")) %>%
    filter(type != "All other makers") %>%
    select(type, date = accident_date, n) %>%
    group_by(type, date) %>%
    summarise(accidents = sum(n), .groups = "drop") %>%
    inner_join(flights_per_quarter) %>%
    filter(date < max(date)) -> accidents_by_make3



