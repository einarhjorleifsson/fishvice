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
#' @param dec desc if an FL-object
#' @param wide A boolean (default TRUE). If FALSE values returned as a single
#' column (named "val"), variable names specified in column named "var"
#'
#' @return A tibble containing key metrics by year and age
#'
flr_rbya <- function(fl, scale = 1, stk, des, wide = TRUE) {

  if(missing(stk)) stk <- FLCore::name(fl)
  if(missing(des)) des <- FLCore::desc(fl)

  bya <-
    fl %>%FLCore:::as.data.frame() %>%
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
  if(!wide) {
    bya %>% return()
  } else {
    bya %>%
      tidyr::spread(var, val) %>%
      dplyr::select(year:iter, cW:sW, everything()) %>%
      return()
  }

}


