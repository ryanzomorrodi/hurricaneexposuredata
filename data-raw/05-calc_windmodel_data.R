## Be sure to re-build package after running 01 and 02 and before
## running this

library(dplyr)

data(hurr_tracks, package = "hurricaneexposuredata")
storms <- unique(hurr_tracks$usa_atcf_id)
storm_id_table <- hurr_tracks %>%
  select(storm_id, usa_atcf_id) %>%
  distinct()

library(stormwindmodel)
data(county_points, package = "stormwindmodel")

library(devtools)
library(dplyr)

# parsing error was present in the package's code
summarize_grid_winds_fix <- function (grid_winds, gust_duration_cut = 20, sust_duration_cut = 20, 
    tint = 0.25) {
    calc_sust_dur <- function(wind) {
        60 * tint * sum(wind > sust_duration_cut, na.rm = TRUE)
    }
    calc_gust_dur <- function(wind) {
        60 * tint * sum(wind > gust_duration_cut, na.rm = TRUE)
    }
    grid_wind_summary <- tibble::tibble(gridid = colnames(grid_winds),
        date = rownames(grid_winds)[apply(grid_winds,
            MARGIN = 2, FUN = which.max)], vmax_sust = apply(grid_winds,
            MARGIN = 2, FUN = max, na.rm = TRUE), vmax_gust = .data$vmax_sust *
            1.49, sust_dur = apply(grid_winds, MARGIN = 2, FUN = calc_sust_dur),
        gust_dur = apply(grid_winds, MARGIN = 2, FUN = calc_gust_dur)) %>%
        dplyr::mutate(date = ifelse(.data$vmax_sust ==
            0, NA, .data$date))
    return(grid_wind_summary)
}

get_grid_winds <- function (hurr_track = stormwindmodel::floyd_tracks, grid_df = stormwindmodel::county_points, 
    tint = 0.25, gust_duration_cut = 20, sust_duration_cut = 20, 
    max_dist = 2222.4) 
{
    grid_winds <- calc_grid_winds(hurr_track = hurr_track, grid_df = grid_df, 
        tint = tint, max_dist = max_dist)
    grid_winds_summary <- summarize_grid_winds_fix(grid_winds = grid_winds$vmax_sust, 
        gust_duration_cut = gust_duration_cut, sust_duration_cut = sust_duration_cut, 
        tint = tint)
    return(grid_winds_summary)
}

calc_windmodel_data <- function(this_storm, centers) {
  print(this_storm)
  storm_track <- subset(hurr_tracks, storm_id == this_storm)
  get_grid_winds(hurr_track = storm_track,
                          grid_df = centers) %>%
    dplyr::mutate(usa_atcf_id = filter(storm_id_table, storm_id == this_storm) %>% pluck("usa_atcf_id"),
                  storm_id = this_storm) %>%
    mutate(storm_id = this_storm) %>%
    relocate(storm_id, usa_atcf_id, .before = everything()) %>%
    rename(geoid = gridid)
}

# usethis::use_data(storm_winds, overwrite = TRUE)
