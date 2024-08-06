library(tidyverse)

outage_filepaths <- Sys.glob("data/eaglei/eaglei_*")

# read all the data and combine it together
outage_data <- map(outage_filepaths, read_csv) %>%
  bind_rows()

outage_date_indexes <- outage_data %>%
  mutate(year = as.integer(year(run_start_time))) %>%
  group_by(year) %>%
  mutate(start_index = row_number()) %>%
  # distinct gets the first row of a run_start_time, so the start_index becomes
  # the row number of the first row for an interval
  distinct(run_start_time, .keep_all = TRUE) %>%
  select(run_start_time, year, start_index) %>%
  # end index is the start_index of the next interval - 1, except the last 
  # end index which is just the number of rows
  mutate(end_index = lead(start_index, default = n() + 1L) - 1L) %>%
  ungroup()

save(outage_date_indexes, file = "data/eaglei/outage_date_indexes.rda")
