library(tidyverse)

outage_filepaths <- Sys.glob("data/eaglei/eaglei_*")

outage_data <- map(outage_filepaths, read_csv) %>%
  bind_rows()

outage_date_indexes <- outage_data %>%
  mutate(year = as.integer(year(run_start_time))) %>%
  group_by(year) %>%
  mutate(start_index = row_number()) %>%
  distinct(run_start_time, .keep_all = TRUE) %>%
  select(run_start_time, year, start_index) %>%
  mutate(end_index = lead(start_index, default = n() + 1L) - 1L) %>%
  ungroup()

save(outage_date_indexes, file = "data/eaglei/outage_date_indexes.rda")
