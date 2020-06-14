#' @title get_data
#' @name get_data
#' @description Retrieve state vectors for a specific plane over an optionally specified amount of time
#'
#' @param username Your 'OpenSky Network' username.
#' @param password Your 'OpenSky Network' password.
#' @param icao24 Unique ICAO 24-bit address of the transponder in hex string
#' representation. All letters need to be lower case.
#' @param start_time Unix time in seconds since epoch, and when to start collecting data for the plane.
#' Does not need to be specified if user only want current and/or state vectors.
#' @param duration Amount of time (in seconds) for which to collect live data. Does not need to be
#' specified if user only wants past and/or current state vectors
#'
#' @return An sf object of state vectors for every 5-second snapshot, from the start time to end of duration.
#'
#'
#' @examples
#' \dontrun{get_data(username = "your_username", password = "your_password", icao24 = "a8699a",
#'  start_time = 1592100547, duration = 300)}
#'
#' @export
#' @import openskyr, httr, sf, rjson, dplyr, jsonlite, RCurl, plyr

get_data <- function(username, password, icao24, start_time = NULL, duration = NULL) {
  if (is.null(start_time) && is.null(duration)) {
    current_time = floor(as.numeric(as.POSIXct(Sys.time())))
    state_vectors_sf = get_past_data(username, password, current_time, icao24)
  }
  else if (is.null(duration)) {
    state_vectors_sf = get_past_data(username, password, start_time, icao24)
  }
  else if (is.null(start_time)) {
    state_vectors_sf = get_live_data(username, password, duration, icao24)
  }
  else {
    past_state_vectors_sf = get_past_data(username, password, start_time, icao24)
    new_state_vectors_sf = get_live_data(username, password, duration, icao24)
    state_vectors_df = rbind(past_state_vectors_sf, new_state_vectors_sf)
  }
  return(state_vectors_sf)
}
