#rbx <- fishvice::mup_rbx("/u2/reikn/Tac/2022/01-cod/ass/mup/smx",
#                         wide = FALSE)

# reykjavik to FLStock ---------------------------------------------------------
#' @title Convert rvk-format to FLStock object
#'
#' @description Converts the rby and rbya to FLStock object.
#'
#' @export
#'
#' @param rbx A list with two data.frames named rby and rbya.
#' @param sName Character vector containing name of stock
#' @param sDesc Character vectro containing some description
#' @param pf Partial fishing mortality
#' @param pm Partial natural mortality
#' @param aF1 Lower reference age
#' @param aF2 Upper reference age
#'
#' @note TO DO: check the unit stuff
#'
rbx_to_flstock <- function(rbx, sName = "nn", sDesc = "none", pf = 0, pm = 0,aF1 = 5, aF2 = 10) {

  rbya <- rbx$rbya
  rby <- rbx$rby

  a1 <- min(rbya$age)
  a2 <- max(rbya$age)
  nAge <- length(a1:a2)

  y1 <- min(rbya$year)
  y2 <- max(rbya$year)

  fls <-
    rbya %>%
    dplyr::select(year, age, oC, cW, n, m, mat, f) %>%
    # this is brute force
    tidyr::drop_na() %>%
    tidyr::gather(rvk, data, -c(year, age)) %>%
    dplyr::left_join(lexicon) %>%
    dplyr::select(slot, age, year, data) %>%
    FLCore::as.FLStock()

  return(fls)

}
