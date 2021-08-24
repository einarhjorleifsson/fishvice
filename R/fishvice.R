#' Results by year
#'
#' @param o An object of class sam, FLSAM or FLStock
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_rby <- function(o) {

  if(class(o)[[1]] == "sam")  return(sam_rby(o))
  if(class(o)[[1]] == "FLSAM") return(fls_rby(o))
  if(class(o)[[1]] == "FLStock") return(flr_rby(o))

}

#' Results by year and age
#'
#' @param o An object of class FLSAM or FLStock
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_rbya <- function(o) {

  if(class(o)[[1]] == "sam") return(sam_rbya(o))
  if(class(o)[[1]] == "FLSAM") return(fls_rbya(o))
  if(class(o)[[1]] == "FLStock") return(flr_rbya(o))


}

#' Observed, predicted and residuals
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_opr <- function(o) {


  #if(class(o)[[1]] == "FLStock") return(flr_rby(o))
  if(class(o)[[1]] == "sam") return(sam_opr(o))
  if(class(o)[[1]] == "FLSAM") return(fls_opr(o))

}

#' Obtain key metrics
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#'
#' @return A list containing tibbles
#' @export
#'
fv_rbx <- function(o) {

  list(rby  = fv_rby(o),
       rbya = fv_rbya(o),
       opr  = fv_opr(o)) %>%
    return()

}
