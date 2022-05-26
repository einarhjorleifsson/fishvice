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
#' @param fage A vector containing lower and upper fishing mortality reference age
#'
#' @note TO DO: check the unit stuff
#'
rbx_to_flstock <- function(rbx, sName = "nn", sDesc = "none", pf = 0, pm = 0, fage = c(5, 10)) {

  rbya <- rbx$rbya

  cn <- colnames(rbya)

  a1 <- min(rbya$age)
  a2 <- max(rbya$age)
  nAge <- length(a1:a2)

  y1 <- min(rbya$year)
  y2 <- max(rbya$year)
  nYear <- length(y1:y2)

  # Note: pf not in rbya
  if(!stringr::str_detect(cn, "pf") %>% any()) {
    if(missing(pf)) pf <- 0
    if(length(pf) == 1) pf <- rep(pf, nAge)
    pf <- rep(pf, nYear)
    rbya$pf <- pf
  }
  if(!stringr::str_detect(cn, "pm") %>% any()) {
    if(missing(pm)) pm <- 0
    if(length(pm) == 1) pm <- rep(pm, nAge)
    pm <- rep(pm, nYear)
    rbya$pm <- pm
  }


  fls <-
    rbya %>%
    dplyr::select(year, age, oC, cW, ssbW, n, m, mat, f, pf, pm) %>%
    # this is brute force, user has to work on this upstream
    # tidyr::drop_na() %>%
    tidyr::gather(rvk, data, -c(year, age)) %>%
    dplyr::mutate(data = tidyr::replace_na(data, 0),
                  units = ifelse(rvk == "f", "f", NA_character_)) %>%
    dplyr::left_join(lexicon, by = "rvk") %>%
    dplyr::select(slot, age, year, data, units) %>%
    FLCore::as.FLStock()

  # units(FLCore::harvest(fls)) <- "f"

    # svolitid ut ur ku
  FLCore::catch(fls) <- rbx$rby$oY

  # NOTE: what if catch.n and catch.wt are not in input??
  if(FLCore::landings.n(fls)  %>% sum(na.rm = TRUE) == 0) FLCore::landings.n(fls)   <- FLCore::catch.n(fls)
  if(FLCore::landings.wt(fls) %>% sum(na.rm = TRUE) == 0) FLCore::landings.wt(fls) <- FLCore::catch.wt(fls)
  if(FLCore::discards.n(fls)  %>% sum(na.rm = TRUE) == 0) FLCore::discards.n(fls)   <- 0
  if(FLCore::discards.wt(fls) %>% sum(na.rm = TRUE) == 0) FLCore::discards.wt(fls)  <- 0

  if(FLCore::discards(fls) %>% sum(na.rm = TRUE) == 0) FLCore::discards(fls) <- 0
  if(FLCore::landings(fls) %>% sum(na.rm = TRUE) == 0) FLCore::landings(fls) <- FLCore::catch(fls)

  if(!missing(fage)) FLCore::range(fls, c("minfbar", "maxfbar")) <- fage

  return(fls)

}


#' rbx_to_flindex
#'
#' @param rbx xxx
#' @param sur xxx
#' @param time xxx
#'
#' @return FLIndex
#' @export
#'
rbx_to_flindex <- function(rbx, sur = "U1", time = c(3/12, 3.5/12)) {

  opr <-
    rbx$opr %>%
    dplyr::filter(fleet == sur) %>%
    dplyr::select(year, age, o) %>%
    # NOTE: need to check this convention
    dplyr::mutate(o = exp(o)) %>%
    tidyr::drop_na()
  ages  <- opr %>% dplyr::pull(age)  %>% unique() %>% sort()
  years <- opr %>% dplyr::pull(year) %>% unique() %>% sort()

  fli <-
    opr %>%
    tidyr::spread(age, o, fill = 0) %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    t() %>%
    FLCore::FLQuant(dimnames = list(age = ages, year = years)) %>%
    FLCore::FLIndex(index = ., name = sur)
  range(fli)[c('startf', 'endf')] <- time

  return(fli)

}


