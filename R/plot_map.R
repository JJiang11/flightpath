#' @title plot_map
#' @name plot_map
#' @description Creates and returns an interactive map object displaying plane's path and chosed variable information.
#' @param data sf of linestrings and interpolated nonspatial data.
#' @param variable String of variable that user wants to respresent by color on the linestrings.
#' @param view Boolean (default TRUE) that specifies if the map returned should be an interactive view map
#' or a stationary plot map, the latter of which conains cartographic elements.
#'
#' @return tmap of plane path on top of an OpenTopoMap basemap (view) or polygon world map (plot).
#'
#' @examples
#' \dontrun{plot_map(data, "velocity")}
#'
#' @import tmap
#' @import sf
#' @import plyr
#' @import leaflet
#' @import spData
#' @export

plot_map <- function(data, variable, view = TRUE) {
  if (!(view)) {
    tmap_mode("plot")

    bound_box = st_bbox(data)
    bound_box[1] = bound_box[1] - 2
    bound_box[2] = bound_box[2] - 2
    bound_box[3] = bound_box[3] + 2
    bound_box[4] = bound_box[4] + 2

    map <- tm_shape(world, bbox = bound_box) +
      tm_polygons() +
      tm_shape(data[variable]) +
      tm_lines(col = variable, lwd = 5) +
      tm_layout(title = paste(data$icao24[1], "Route", sep = " ")) +
      tm_compass() +
      tm_scale_bar()

    return(map)
  }

  tmap_mode("view")

  map <- tm_basemap(leaflet::providers$OpenTopoMap, alpha = 0.5) +
    tm_shape(data[variable]) +
    tm_lines(col = variable, lwd = 5) +
    tm_layout(title = paste(data$icao24[1], "Route", sep = " "))

  return(map)
}

