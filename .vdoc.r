#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)
library(tigris)
library(sf)

center_tr10 <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR.txt") %>%
    mutate(
        gridid = str_c(STATEFP, COUNTYFP, TRACTCE),
        glat = LATITUDE,
        glon = LONGITUDE
    ) %>%
    mutate(
        glandsea = st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = 4326)  %>%
            st_intersects(st_transform(states(cb = TRUE), crs = 4326), sparse = FALSE) %>%
            {rowSums(.) > 0L}
    ) %>%
    select(gridid, glat, glon, glandsea)
#
#
#
source("data-raw/03-calculate_closest_distance.R")
source("data-raw/05-calc_windmodel_data.R")
source("data-raw/08-idw_precip.R")
source("data-raw/10-eaglei_county.R")
#
#
#
#
#
states_fips <- c("37")
neighbor_states <- c("GA", "NC", "SC", "TN", "VA")
storm_id <- "Matthew-2016"

# get centers
center_tr10_selected <- center_tr10 %>%
    filter(str_sub(gridid, 1, 2) %in% states_fips)

Matthew16 <- list()
Matthew16$closest_dis_tr10 <- calc_closest_dist(storm_id, center_tr10_selected)
Matthew16$windmodel_tr10 <- calc_windmodel_data(storm_id, center_tr10_selected)
Matthew16$prcp_tr10 <- calc_idw_pred(neighbor_states, center_tr10_selected, storm_id, 12)
Matthew16$outages_cnty <- calc_outage(storm_id, states_fips, days_after = 5)
#
#
#
#
#
states_fips <- c("48", "22")
neighbor_states <- c("NM", "CO", "OK", "AR", "MS", "TX", "LA")
storm_id <- "Harvey-2017"

# get centers
center_tr10_selected <- center_tr10 %>%
    filter(str_sub(gridid, 1, 2) %in% states_fips)

Harvey17 <- list()
Harvey17$closest_dis_tr10 <- calc_closest_dist(storm_id, center_tr10_selected)
Harvey17$windmodel_tr10 <- calc_windmodel_data(storm_id, center_tr10_selected)
Harvey17$prcp_tr10 <- calc_idw_pred(neighbor_states, center_tr10_selected, storm_id, 12)
Harvey17$outages_cnty <- calc_outage(storm_id, states_fips, days_after = 5)
#
#
#
#
#
states_fips <- c("37")
neighbor_states <- c("GA", "NC", "SC", "TN", "VA")
storm_id <- "Florence-2018"

# get centers
center_tr10_selected <- center_tr10 %>%
    filter(str_sub(gridid, 1, 2) %in% states_fips)

Florence18 <- list()
Florence18$closest_dis_tr10 <- calc_closest_dist(storm_id, center_tr10_selected)
Florence18$windmodel_tr10 <- calc_windmodel_data(storm_id, center_tr10_selected)
Florence18$prcp_tr10 <- calc_idw_pred(neighbor_states, center_tr10_selected, storm_id, 12)
Florence18$outages_cnty <- calc_outage(storm_id, states_fips, days_after = 5)
#
#
#
#
#
storms <- list(
    "Matthew16" = Matthew16, 
    "Harvey17" = Harvey17, 
    "Florence18" = Florence18
) 

dir.create("output")
names(storms) %>%
    walk(function (storm) {
        tables <- names(storms[[storm]])
        dir.create(str_c("output/", storm))
        walk(tables, function (table) {
            print(storms[[storm]][[table]])
            write_csv(storms[[storm]][[table]], str_c("output/", storm, "/", table, ".csv"))
            
        })
    })
#
#
#
