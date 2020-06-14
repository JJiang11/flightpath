#' @title lonlat2UTM
#' @name lonlat2UTM
#' @description calculates EPSG code associated with any point on the planet, from Lovelace 6.3
#' @param lonlat longitude/latitude coordinates
#' @return UTM code
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}
