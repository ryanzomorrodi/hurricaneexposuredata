library(tidyverse)

load("data/outage_date_indexes.rda")
load("data/hurr_tracks.rda")


calc_outage <- function(storm_id, states_fips, days_after = 5) {
  # get usa_atcf_id
  usa_atcf_id <- hurr_tracks %>%
    filter(.data$storm_id == .env$storm_id) %>% 
    pluck("usa_atcf_id", 1)
  # get first day and last day of storm + days_after
  storm_dates <- hurr_tracks %>%
    filter(.data$storm_id == .env$storm_id) %>%
    pluck("date")
  first_date <- ymd_hm(min(storm_dates))
  last_date <- ymd_hm(max(storm_dates)) + days(days_after)

  # get the first and last index of the dates for each year of the storm (usually only one year)
  storm_date_indexes <- outage_date_indexes %>%
    filter(run_start_time %within% interval(first_date, last_date)) %>%
    group_by(year) %>%
    summarize(start_index = first(start_index), end_index = last(end_index))

  # read the outage data for the indexes gotten
  outage_data <- pmap(storm_date_indexes, function(year, start_index, end_index) {
    read_csv(
      str_c("data-raw/eaglei/eaglei_outages_", year, ".csv"),
      skip = start_index,
      n_max = end_index - start_index + 1,
      col_names = names(read_csv(str_c("data-raw/eaglei/eaglei_outages_", year, ".csv"), n_max = 0))
    )
  }) %>%
    bind_rows()

  # code modified from Uri_Map.R
  mcc <- readr::read_csv("data-raw/eaglei/MCC.csv") %>%
    rename(GEOID = County_FIPS) %>%
    group_by(GEOID) %>% 
    summarise(mcc = sum(Customers, na.rm = TRUE)) %>%
    mutate(GEOID = str_pad(as.character(GEOID), width = 5, pad = "0", side = "left"))

  # combine estimates of customers and outage data, calculate the percent of outage
  # at each 15 min interval (if greater than 1, then force to 1), from that calculate
  # the maximimum and the total number of hours of outage per customer
  outage_data %>%
    full_join(x = mcc, by = c("GEOID" = "fips_code")) %>%
    group_by(GEOID) %>% 
    mutate(
      customers_out = if_else(is.na(customers_out), 0, customers_out),
      mcc = if_else(is.na(mcc), 0, mcc),
      pct_out = if_else(customers_out/mcc > 1, 1, customers_out/mcc)) %>%
    summarise(
      max_pct_out = max(pct_out),
      avg_customer_hrs_out = sum(pct_out) * 0.25
    ) %>%
    # the rest of this is just standardizing the output
    filter(str_sub(GEOID, 1, 2) %in% states_fips) %>%
    mutate(
      storm_id = storm_id,
      usa_atcf_id = usa_atcf_id
    ) %>%
    select(
      storm_id,
      usa_atcf_id,
      geoid = GEOID,
      max_pct_out,
      avg_customer_hrs_out 
    )
}
