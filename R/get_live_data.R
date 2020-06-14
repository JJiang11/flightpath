#' @title get_live_data
#' @name get_live_data
#' @description Retrive all flight vectors every 5 seconds
#'  starting now up to a specified time into the future
#' @param username Your 'OpenSky Network' username.
#' @param password Your 'OpenSky Network' password.
#' @param duration Amount of time (in seconds) for which to collect live data
#' @param icao24 Optional. Unique icao24 identifier for aircraft
#'
#' @return If icao24 is missing, A non-spatial dataframe of all state vectors retrieved during the collection period.
#'   if icao24 specified, A geocoded sf is returned instead.
#'
#' @examples
#' \dontrun{collect_live_data(username = "your_username", password = "your_password",
#'  duration = 30)}
#'
#' @export
#' @import openskyr, sf

get_live_data <- function(username, password, duration, icao24 = NULL, ...) {
  current_time = as.numeric(as.POSIXct(Sys.time()))
  start_time = current_time
  state_vectors_df = get_state_vectors(username = username, password = password, ...)
  icao24_arg = icao24
  if(!is.null(icao24_arg)){
    state_vectors_df = state_vectors_df %>%
      filter(icao24 == icao24_arg)
  }
  Sys.sleep(5)
  while (current_time < start_time + duration) {
    next_df = get_state_vectors(username = username, password = password, ...)
    if(!is.null(icao24_arg)){
      next_df = next_df %>%
        filter(icao24 == icao24_arg)
    }
    state_vectors_df = rbind(state_vectors_df, next_df)
    Sys.sleep(5)
    current_time = as.numeric(as.POSIXct(Sys.time()))
  }
  if(!is.null(icao24_arg)){
    proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    state_vectors_sf <- st_as_sf(state_vectors_df, coords = c("longitude", "latitude"), crs = proj)
    return(state_vectors_sf)
  }
  return(state_vectors_df)
}
