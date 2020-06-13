#' @title collect_live_data
#' @name collect_live_data
#' @description Retrive all flight vectors every 5 seconds
#'  starting now up to a specified time into the future
#' @param username Your 'OpenSky Network' username.
#' @param password Your 'OpenSky Network' password.
#' @param duration Amount of time (in seconds) for which to collect live data
#'
#' @return A dataframe of all state vectors retrieved during the collection period
#'
#' @examples
#' \dontrun{collect_live_data(username = "your_username", password = "your_password",
#'  duration = 30)}
#'
#' @export
#' @import openskyr

collect_live_data <- function(username, password, duration) {
  current_time = as.numeric(as.POSIXct(Sys.time()))
  start_time = current_time
  state_vectors_df = get_state_vectors(username = username, password = password)
  Sys.sleep(5)
  while (current_time < start_time + duration) {
    state_vectors_df <- rbind(state_vectors_df, get_state_vectors(username = username, password = password))
    Sys.sleep(5)
    current_time = as.numeric(as.POSIXct(Sys.time()))
  }
  return(state_vectors_df)
}
