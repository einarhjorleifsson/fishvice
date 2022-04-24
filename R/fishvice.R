#' Results by year
#'
#' @param o An object of class sam, FLSAM or FLStock
#' @param scale A scale for the scaleable variables
#' @param run Name of the run
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_rby <- function(o, scale, run) {

  if(missing(scale)) scale <- 1

  if(class(o)[[1]] == "sam")  return(sam_rby(o, scale = scale, run = run))
  if(class(o)[[1]] == "FLSAM") return(fls_rby(o))
  if(class(o)[[1]] == "FLStock") return(flr_rby(o))
  if(class(o)[[1]] == "character") return(mup_rby(o, scale = scale, run = run))

}

#' Results by year and age
#'
#' @param o An object of class FLSAM or FLStock
#' @param scale A scale for the scaleable variables
#' @param run Name of the run
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_rbya <- function(o, scale, run) {

  if(missing(scale)) scale <- 1

  if(class(o)[[1]] == "sam") return(sam_rbya(o, scale = scale, run = run))
  if(class(o)[[1]] == "FLSAM") return(fls_rbya(o))
  if(class(o)[[1]] == "FLStock") return(flr_rbya(o))
  if(class(o)[[1]] == "character") return(mup_rbya(o, scale = scale, run = run))


}

#' Observed, predicted and residuals
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#' @param scale A scale for the scaleable variables
#' @param run Name of the run
#'
#' @return A tibble containing key metric by year
#' @export
#'
fv_opr <- function(o, scale, run) {

  if(missing(scale)) scale <- 1


  #if(class(o)[[1]] == "FLStock") return(flr_rby(o))
  if(class(o)[[1]] == "sam")   return(sam_opr(o, scale = scale, run = run))
  if(class(o)[[1]] == "FLSAM") return(fls_opr(o))
  if(class(o)[[1]] == "character") return(mup_opr(o, run = run))

}

#' Parameters
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#' @param scale A scale for the scaleable variables
#' @param run Name of the run
#'
#' @return A tibble containing key metrics
#' @export
#'
fv_par <- function(o, scale, run) {

  if(missing(scale)) scale <- 1


  #if(class(o)[[1]] == "FLStock")
  if(class(o)[[1]] == "sam")   return(sam_partable(o, run))
  #if(class(o)[[1]] == "FLSAM")
  if(class(o)[[1]] == "character") return(mup_par(o, run = run))

}

#' Obtain key metrics
#'
#' @param o An object of class sam, FLSAM or a directory path (for muppet)
#' @param scale A scale for the scaleable variables
#' @param run Name of the run
#'
#' @return A list containing tibbles
#' @export
#'
fv_rbx <- function(o, scale, run) {

  if(missing(scale)) scale <- 1

  if(!class(o)[[1]] %in% c("sam", "FLSAM", "FLStock", "character")) {
    stop(paste0("Object passed to the function 'fv_rbx' is of class '",
                class(o),
                "'.\nObjects passed has to be of class 'sam', 'FLSAM' ,'FLStock'. or a character"))
  }

  list(rby  = fv_rby(o, scale = scale, run = run),
       rbya = fv_rbya(o, scale = scale, run = run),
       opr  = fv_opr(o, scale = scale, run = run),
       par  = fv_par(o, run = run)) %>%
    return()

}
