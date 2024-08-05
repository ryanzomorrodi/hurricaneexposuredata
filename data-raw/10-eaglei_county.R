library(tidyverse)

load("data/eaglei/outage_date_indexes.rda")
load("data/hurr_tracks.rda")


get_outage <- function(storm_id, days_after = 5) {
  storm_dates <- hurr_tracks %>%
    filter(.data$storm_id == .env$storm_id) %>%
    pluck("date")
  
  first_date <- ymd_hm(min(storm_dates))
  last_date <- ymd_hm(max(storm_dates)) + days(days_after)

  storm_date_indexes <- outage_date_indexes %>%
    filter(run_start_time %within% interval(first_date, last_date)) %>%
    group_by(year) %>%
    summarize(start_index = first(start_index), end_index = last(end_index))

  outage_data <- pmap(storm_date_indexes, function(year, start_index, end_index) {
    read_csv(
      str_c("data/eaglei/eaglei_outages_", year, ".csv"),
      skip = start_index,
      n_max = end_index - start_index + 1,
      col_names = names(read_csv(str_c("data/eaglei/eaglei_outages_", year, ".csv"), n_max = 0))
    )
  }) %>%
    bind_rows()

  # code from Uri_Map.R 
  outage_data_processed <- outage_data %>%
    group_by(fips_code) %>% 
    mutate(sum = if_else(is.na(customers_out), 0, customers_out)) %>%
    summarise(
      max_out = max(customers_out, na.rm = TRUE),
      cust_hours_out = sum(customers_out, na.rm = TRUE)*0.25
    )

  mcc <- readr::read_csv("data/eaglei/MCC.csv") %>%
    rename(GEOID = County_FIPS) %>%
    group_by(GEOID) %>% 
    summarise(mcc = sum(Customers, na.rm = TRUE)) %>%
    mutate(GEOID = str_pad(as.character(GEOID), width = 5, pad = "0", side = "left"))

  full_join(mcc, outage_data_processed, by = c("GEOID" = "fips_code")) %>% 
    mutate(max_out = if_else(is.na(max_out), 0, max_out),
      cust_hours_out = if_else(is.na(cust_hours_out), 0, cust_hours_out),
      max_pct_out = if_else(max_out/mcc> 1, 1, max_out/mcc),
      max_pct_out = if_else(is.na(max_pct_out), 0, max_pct_out)) 

}
