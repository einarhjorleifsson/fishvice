#' @title Set all column names in a data_frame to lower
#'
#' @description A pipe friendly function
#'
#' @export
#'
#' @param . No argument needed

to_lower <- function(.) {
  setNames(., nm = tolower(names(.)))
}

#' @title Returns the lon for a statistical square (tilkynningaskildureitur)
#'
#' @description A dplyr-friendly variant of the geo::r2d, returning only a
#' vector of longitudes. To be used inside mutate.
#'
#' @export
#'
#' @param x Numerical vector containing statistical square
r2lon <-  function(x) {
  -((x-floor(x/100)*100)%%50 + 0.5)
}

#' @title Returns the latitude for a statistical square (tilkynningaskildureitur)
#'
#' @description A dplyr-friendly variant of the geo::r2d, returning only a
#' vector of latitudes. To be used inside mutate.
#'
#' @export
#'
#' @param x Numerical vector containing statistical square
r2lat <- function(x) {
  lat <- floor(x/100)
  lon <- (x - lat * 100)%%50
  halfb <- (x - 100 * lat - lon)/100
  lat <- lat + 60 + halfb + 0.25
}
