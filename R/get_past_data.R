#' @title get_past_data
#' @name get_past_data
#' @description Retrieve state vectors for a specific plane if the desired start time is before the current time
#'
#' @param username Your 'OpenSky Network' username.
#' @param password Your 'OpenSky Network' password.
#' @param icao24 Unique ICAO 24-bit address of the transponder in hex string
#' representation. All letters need to be lower case
#' @param start_time Unix time in seconds since epoch, and when to start collecting data for the plane.
#'
#' @return A dataframe of state vectors for every 5-second snapshot, from the start time to current time
#'
#'
#' @examples
#' \dontrun{get_past_data(username = "your_username", password = "your_password",
#' icao24 = 3c4b26, time = (variable with time))}
#'
#' @export
#' @import httr, sf, rjson, dplyr, jsonlite, RCurl, plyr

get_past_data <- function(username, password, icao24, start_time) {
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
  return(state_vectors_df)
}
