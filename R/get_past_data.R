#' @title get_past_data
#' @name get_past_data
#' @description Retrieve state vectors for a specific plane if the desired start time is before the current time
#'
#' @param username Your 'OpenSky Network' username.
#' @param password Your 'OpenSky Network' password.
#' @param start_time Unix time in seconds since epoch, and when to start collecting data for the plane.
#' @param icao24 Unique ICAO 24-bit address of the transponder in hex string
#' representation. All letters need to be lower case
#'
#' @return A dataframe of state vectors for every 5-second snapshot, from the start time to current time
#'
#'
#' @examples
#' \dontrun{get_past_data(username = "your_username", password = "your_password",
#'  time = 1592100547, icao24 = 3c4b26)}
#'
#' @export
#' @import httr, sf, rjson, dplyr, jsonlite, RCurl, plyr

get_past_data <- function(username, password, start_time, icao24) {
  test_time = start_time
  current_time = floor(as.numeric(as.POSIXct(Sys.time())))
  url1 = "https://opensky-network.org/api/states/all?time="
  url2 = "&icao24="
  url = paste(url1, current_time, url2, icao24, sep = "")
  state_vectors_df <- as.data.frame(fromJSON(url))
  while (test_time < current_time) {
    url = paste(url1, test_time, url2, icao24, sep = "")
    state_vectors_df <- rbind(state_vectors_df, as.data.frame(fromJSON(url)))
    test_time = test_time + 5
  }
  state_vectors_df <- rename(state_vectors_df, c("states.1" = "icao24", "states.2" = "callsign",
                             "states.3" = "origin_country", "states.4" = "time_position",
                             "states.5" = "last_contact", "states.6" = "longitude",
                             "states.7" = "latitude", "states.8" = "baro_altitude",
                             "states.9" = "on_ground", "states.10" = "velocity",
                             "states.11" = "true_track", "states.12" = "vertical_rate",
                             "states.13" = "sensors", "states.14" = "geo_altitude",
                             "states.15" = "squawk", states.16 = "spi", "states.17" = "position_source"))
  state_vectors_df$time = NULL
  state_vectors_df$icao24 = as.character(state_vectors_df$icao24)
  state_vectors_df$callsign = as.character(state_vectors_df$callsign)
  state_vectors_df$time_position = as.character(state_vectors_df$time_position)
  state_vectors_df$last_contact = as.numeric(as.character(state_vectors_df$last_contact))
  state_vectors_df$baro_altitude = as.numeric(as.character(state_vectors_df$baro_altitude))
  state_vectors_df$on_ground = as.logical(as.character(state_vectors_df$on_ground))
  state_vectors_df$velocity = as.numeric(as.character(state_vectors_df$velocity))
  state_vectors_df$true_track = as.numeric(as.character(state_vectors_df$true_track))
  state_vectors_df$vertical_rate = as.numeric(as.character(state_vectors_df$vertical_rate))
  state_vectors_df$sensors = as.null(as.character(state_vectors_df$sensors))
  state_vectors_df$geo_altitude = as.numeric(as.character(state_vectors_df$geo_altitude))
  state_vectors_df$squawk = as.character(state_vectors_df$squawk)
  state_vectors_df$spi = as.logical(as.character(state_vectors_df$spi))
  state_vectors_df$position_source = as.integer(as.character(state_vectors_df$position_source))
  state_vectors_df$longitude = as.numeric(as.character(state_vectors_df$longitude))
  state_vectors_df$latitude = as.numeric(as.character(state_vectors_df$latitude))
  proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  state_vectors_sf <- st_as_sf(state_vectors_df, coords = c("longitude", "latitude"), crs = proj)
  return(state_vectors_sf)
}
