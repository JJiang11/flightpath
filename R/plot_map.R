#' @title plot_map
#' @name plot_map
#' @description Creates and returns an interactive map object displaying plane's path and chosed variable information.
#' @param data sf of linestrings and interpolated nonspatial data.
#' @param variable String of variable that user wants to respresent by color on the linestrings.
#'
#' @return Interactive tmap map of plane path on top of an OpenTopoMap basemap
#'
#' @examples
#' \dontrun{plot_map(data, "velocity")}
#'
#' @import tmap
#' @import sf
#' @import leaflet
#' @export

plot_map <- function(data, variable) {
  tmap_mode("view")
  map <- tm_basemap(leaflet::providers$OpenTopoMap, alpha = 0.5) +
    tm_shape(data[variable]) +
    tm_lines(col = variable, lwd = 5)

  return(map)
}
