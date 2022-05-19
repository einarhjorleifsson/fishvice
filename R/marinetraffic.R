# We can search mt by e.g. imo using the browser:
# https://www.marinetraffic.com/en/ais/details/ships/imo:9192404
# the above takes us to a new page, in this instance:
# https://www.marinetraffic.com/en/ais/details/ships/shipid:294305/mmsi:251507000/imo:9192404/vessel:ARNI_FRIDRIKSSON
# conceptually, if not a better way is available if one could get this response path we could then extract the shipid
# and then pass that to the above function
# in R the steps may be:
# url <- "https://www.marinetraffic.com/en/ais/details/ships/imo:9192404"
# send as "browser" function and get the url that this "call" sends the user to

#' Vessel info from marinetraffic.com
#'
#' @param mtid Marine traffic vessel id
#'
#' @return A tibble
#' @export
mt_vesselinfo <- function(mtid = 294305) {

  paste0("https://www.marinetraffic.com/en/vesselDetails/vesselInfo/shipid:",
         mtid) %>%
    jsonlite::read_json() %>%
    purrr::as_vector() %>%
    tibble::tibble(var = names(.)) %>%
    dplyr::select(var, val = 1)

}

