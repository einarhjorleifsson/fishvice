# reykjavik to FLR -------------------------------------------------------------
# rbx <- fishvice::mup_rbx("/u2/reikn/Tac/2022/01-cod/ass/mup/smx",
#                          wide = FALSE)


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
rbx_to_flstock <- function(rbx, sName = "nn", sDesc = "none", pf = 0, pm = 0, aF1 = 5, aF2 = 10) {

  a1 <- min(rbya$age)
  a2 <- max(rbya$age)
  nAge <- length(a1:a2)

  y1 <- min(rbya$year)
  y2 <- max(rbya$year)

  fls <-
    rbx$rbya %>%
    dplyr::select(year, age, oC, cW, ssbW, n, m, mat, f) %>%
    # this is brute force, user has to work on this upstream
    # tidyr::drop_na() %>%
    tidyr::gather(rvk, data, -c(year, age)) %>%
    dplyr::mutate(data = tidyr::replace_na(data, 0)) %>%
    dplyr::left_join(lexicon, by = "rvk") %>%
    dplyr::select(slot, age, year, data) %>%
    FLCore::as.FLStock()

  units(FLCore::harvest(fls)) <- "f"
  # check if pF and pM is in rbya, if not use input vector
  #  if missing, assume 0
  # need to revisit, if not atomic does one need the rep
  if(FLCore::harvest.spwn(fls) %>% is.na() %>% all()) {
    FLCore::harvest.spwn(fls) <- rep(pf, nAge)
    units(FLCore::harvest.spwn(fls)) <- "f"
  }
  if(FLCore::m.spwn(fls) %>% is.na() %>% all()) {
    FLCore::m.spwn(fls) <- rep(pm, nAge)
    units(FLCore::m.spwn(fls)) <- "f"
  }

  # svolitid ut ur ku
  FLCore::catch(fls) <- rby$oY

  # NOTE: what if catch.n and catch.wt are not in input??
  if(FLCore::landings.n(fls)  %>% sum(na.rm = TRUE) == 0) FLCore::landings.n(fls)   <- FLCore::catch.n(fls)
  if(FLCore::landings.wt(fls) %>% sum(na.rm = TRUE) == 0) FLCore::landings.wt(fls) <- FLCore::catch.wt(fls)
  if(FLCore::discards.n(fls)  %>% sum(na.rm = TRUE) == 0) FLCore::discards.n(fls)   <- 0
  if(FLCore::discards.wt(fls) %>% sum(na.rm = TRUE) == 0) FLCore::discards.wt(fls)  <- 0

  if(FLCore::discards(fls) %>% sum(na.rm = TRUE) == 0) FLCore::discards(fls) <- 0
  if(FLCore::landings(fls) %>% sum(na.rm = TRUE) == 0) FLCore::landings(fls) <- FLCore::catch(fls)

  return(fls)

}

