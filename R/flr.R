# FLStock object -> rby
flr_rby <- function(fl, scale = 1, stk, des, long = TRUE) {

  if(missing(stk)) stk <- FLCore::name(fl)
  if(missing(des)) des <- FLCore::desc(fl)

  fl %>%
    FLCore:::as.data.frame() %>%
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

#' xxx
#'
#' @param fl An FLStock object
#' @param scale A numeric, specifying re-scaling (default is 1). Not used (yet)
#' @param stk Name of the run, if missing (default) will use the directory name
#' or stock name if FL-object.
#' @param des desc if an FL-object, if missing (default) will use the directory name
#' or stock name if FL-object.
#'
#' @return A tibble containing key metrics by year and age
#'
flr_rbya <- function(fl, scale = 1, stk, des) {




  if(missing(stk) & class(fl)[[1]] == "FLStock") stk <- FLCore::name(fl)  # NOTE: no method for FLStocks
  if(missing(des) & class(fl)[[1]] == "FLStock") des <- FLCore::desc(fl)  # NOTE: no method for FLStocks

  bya <-
    fl %>%
    FLCore:::as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::filter(age != "all") %>%
    dplyr::mutate(age = as.integer(age)) %>%
    dplyr::select(year, age, iter, slot, data) %>%
    dplyr::mutate(slot = dplyr::case_when(slot == "catch.n"  ~ "oC",
                                          slot == "discards.n" ~ "oD",
                                          slot == "landings.n" ~ "oL",
                                          slot == "catch.wt" ~ "cW",
                                          slot == "discards.wt" ~ "dW",
                                          slot == "landings.wt" ~ "lW",
                                          slot == "stock.wt" ~ "sW",
                                          slot == "mat" ~ "mat",
                                          slot == "m" ~ "m",
                                          slot == "harvest.spwn" ~ "pF",
                                          slot == "m.spwn" ~ "pM",
                                          slot == "stock.n" ~ "n",
                                          slot == "harvest" ~ "f",
                                          TRUE ~ slot)) %>%
    dplyr::rename(var = slot,
                  val = data)

  if(!missing(stk)) bya$stk <- stk
  if(!missing(des)) bya$des <- des

  return(bya)

}


