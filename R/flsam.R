# FLSAM object -> rby
fls_rby <- function(fl, scale = 1, stk, des) {

  if(class(fl)[[1]] != "FLSAM")
    stop(paste0('Expected object class "FLSAM"\n',
                'Object passed is of class "', class(fl)), '"')

  if(missing(stk)) stk <- FLCore::name(fl)
  if(missing(des)) des <- FLCore::desc(fl)

  lh <- function(x, variable, scale = 1) {
    x %>%
      tibble::as_tibble() %>%
      dplyr::mutate(est = value / scale,
                    low = lbnd / scale,
                    high = ubnd / scale) %>%
      dplyr::select(var = variable, year, est, low, high)
  }

  dplyr::bind_rows(FLCore::catch(fl) %>% lh("catch", scale = scale),
                   FLCore::rec(fl)   %>% lh("rec", scale = scale),
                   FLCore::ssb(fl)   %>% lh("ssb", scale = scale),
                   FLCore::tsb(fl)   %>% lh("tsb", scale = scale),
                   FLCore::fbar(fl)  %>% lh("fbar"))

}

# FLStock object -> rby
flr_rby <- function(fl, scale = 1, stk, des, long = TRUE) {

  if(missing(stk)) stk <- FLCore::name(fl)
  if(missing(des)) des <- FLCore::desc(fl)

  fl %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::rename(var = slot,
                  val = data) %>%
    dplyr::filter(age == "all") %>%
    dplyr::select(year, iter, var, val) %>%
    dplyr::mutate(year = as.integer(year),
                  iter = as.integer(iter),
                  var = ifelse(var == "stock", "ssb", var),   # have no idea if this is right
                  stock = stk,
                  desc = des)


}




#' flsam_rbya
#'
#' @param fl An FLSAM object
#' @param scale A numeric, specifying re-scaling (default is 1). Not used (yet)
#' @param stk Name of the run, if missing (default) will use the directory name
#' or stock name if FL-object.
#' @param dec desc if an FL-object
#' @param long A boolean (default TRUE). If TRUE values returned as a single
#' column (named "val"), variable names specified in column named "var"
#'
#' @return A tibble containing key metrics by year and age
#'
flr_rbya <- function(fl, scale = 1, stk, des, long = TRUE) {

  if(missing(stk)) stk <- FLCore::name(fl)
  if(missing(des)) des <- FLCore::desc(fl)

  bya <-
    fl %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::filter(age != "all") %>%
    dplyr::mutate(age = as.integer(age)) %>%
    dplyr::select(year, age, iter, slot, data) %>%
    dplyr::mutate(slot = case_when(slot == "catch.n"  ~ "oC",
                                   slot == "catch.wt" ~ "cW",
                                   slot == "discards.n" ~ "oD",
                                   slot == "discards.wt" ~ "dW",
                                   slot == "harvest" ~ "f",
                                   slot == "harvest.spwn" ~ "pF",
                                   slot == "landings.n" ~ "oL",
                                   slot == "landings.wt" ~ "lW",
                                   slot == "m.spwn" ~ "pM",
                                   slot == "stock.n" ~ "n",
                                   slot == "stock.wt" ~ "sW",
                                   TRUE ~ slot),
                  stock = stk,
                  desc  = des) %>%
    dplyr::rename(var = slot,
                  val = data)
  if(long) {
    bya %>% return()
  } else {
    bya %>%
      tidyr::spread(var, val) %>%
      dplyr::select(year:iter, cW:sW, everything()) %>%
      return()
  }

}

flsam_opr <- function(fl, lgs = TRUE, scale = 1) {

  if(class(fl)[[1]] != "FLSAM")
    stop(paste0('Expected object class "FLSAM"\n',
                'Object passed is of class "', class(fl)), '"')

  FLCore::residuals(fl) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(fleet = ifelse(fleet == "catch unique", "catch", fleet)) %>%
    dplyr::rename(o = log.obs, p = log.mdl, r = std.res) %>%
    dplyr::mutate(year = year - 1 + range(fl)[[4]])

}



