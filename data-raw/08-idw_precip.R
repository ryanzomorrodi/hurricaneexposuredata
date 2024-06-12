library(tidyverse)
library(gstat)
library(furrr)
library(progressr)

load("data/hurr_tracks.rda")

stations <- read_fwf("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 
    fwf_cols(
      STATION = c(1, 11), 
      LATITUDE = c(13, 20), 
      LONGITUDE = c(22,30),
      ELEVATION = c(32,37),
      STATE = c(39,40),
      NAME = c(42,71),
      GSN_FLAG = c(73,75),
      HCNCRN_FLAG = c(77, 79),
      WMO_ID = c(81,85)
    )
  )

calc_idw_pred <- function(neighbor_states, centers, this_storm, workers) {
    selected_stations <- stations %>% 
        filter(STATE %in% neighbor_states) %>%
        st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

    centers_sf <- centers  %>%
        st_as_sf(coords = c("glon", "glat"), crs = 4326) 

    storm_dates <- hurr_tracks %>%
        filter(.data$storm_id == this_storm) %>%
        pluck("date")
    first_date <- ymd(str_sub(min(storm_dates), start = 1, end = 8)) - days(5)
    last_date <- ymd(str_sub(max(storm_dates), start = 1, end = 8)) + days(5)
    year <- year(first_date)

    station_data <- read_csv(str_c("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_year/", year, ".csv.gz"),
        col_names = c("STATION", "DATE", "VAR", "VAL", "M_FLAG", "Q_FLAG", "S_FLAG"),
        col_types = cols_only(
            STATION = col_character(),
            DATE = col_date(format = "%Y%m%d"),
            VAR = col_character(),
            VAL = col_double(),
            VAR = col_character(),
            M_FLAG = col_character(),
            Q_FLAG = col_character(),
            S_FLAG = col_character(),
        ),
        lazy = TRUE) %>%
    filter(STATION %in% selected_stations$STATION) %>% 
    filter(DATE %within% interval(first_date, last_date)) %>%
    filter(is.na(Q_FLAG)) %>% # remove failed quality check measurements
    pivot_wider(
        id_cols = c(STATION, DATE), 
        id_expand = TRUE, 
        names_from = VAR,
        values_from = VAL)

    plan(multisession, workers = workers)

    with_progress({
        p <- progressor(steps = as.numeric(last_date - first_date))

        idw_pred_data <- seq(first_date, last_date, by = 1) %>% future_map_dfr(~
            station_data %>%
                left_join(select(selected_stations, STATION)) %>%
                filter(DATE == .x) %>%
                st_as_sf() %>%
                drop_na(PRCP) %>%
                gstat(data = ., formula = PRCP ~ 1, nmax = 8, set = list(idp = 0.5)) %>%
                    predict(centers_sf) %>% 
                    st_drop_geometry() %>%
                    select(var1.pred) %>%
                    rename(PRCPpred = var1.pred) %>%
                    bind_cols(centers_sf, DATE = as_date(.x), .)
        ) %>%
            st_drop_geometry() %>%
            mutate(storm_id = this_storm) %>%
            mutate(usa_atcf_id = filter(hurr_tracks, storm_id == this_storm) %>% 
                pluck("usa_atcf_id", 1)) %>%
            relocate(storm_id, usa_atcf_id, .before = everything()) %>%
            select(-glandsea) %>%
            rename(geoid = gridid)
    })

    return (idw_pred_data)
}