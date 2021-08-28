#' Results by year
#'
#' @param o An object of class sam, FLSAM or FLStock
#' @param scale A scale for the scaleable variables
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_rby <- function(o, scale = 1) {

  if(class(o)[[1]] == "sam")  return(sam_rby(o, scale = scale))
  if(class(o)[[1]] == "FLSAM") return(fls_rby(o))
  if(class(o)[[1]] == "FLStock") return(flr_rby(o))

}

#' Results by year and age
#'
#' @param o An object of class FLSAM or FLStock
#' @param scale A scale for the scaleable variables
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_rbya <- function(o, scale) {

  if(class(o)[[1]] == "sam") return(sam_rbya(o, scale = scale))
  if(class(o)[[1]] == "FLSAM") return(fls_rbya(o))
  if(class(o)[[1]] == "FLStock") return(flr_rbya(o))


}

#' Observed, predicted and residuals
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#' @param scale A scale for the scaleable variables
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_opr <- function(o, scale = 1) {


  #if(class(o)[[1]] == "FLStock") return(flr_rby(o))
  if(class(o)[[1]] == "sam")   return(sam_opr(o, scale = scale))
  if(class(o)[[1]] == "FLSAM") return(fls_opr(o))

}

#' Parameters
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#'
#' @return A tibble containing key metrics
#' @export
#'
fv_par <- function(o, scale = 1) {


  #if(class(o)[[1]] == "FLStock")
  if(class(o)[[1]] == "sam")   return(sam_partable(o))
  #if(class(o)[[1]] == "FLSAM")

}

#' Obtain key metrics
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#' @param scale A scale for the scaleable variables
#'
#' @return A list containing tibbles
#' @export
#'
fv_rbx <- function(o, scale = 1) {

  if(!class(o)[[1]] %in% c("sam", "FLSAM", "FLStock")) {
    stop(paste0("Object passed to the function 'fv_rbx' is of class '",
                class(o),
                "'.\nObjects passed has to be of class 'sam', 'FLSAM' or 'FLStock'."))
  }

  list(rby  = fv_rby(o, scale = scale),
       rbya = fv_rbya(o, scale = scale),
       opr  = fv_opr(o, scale = scale),
       par  = fv_par(o)) %>%
    return()

}
