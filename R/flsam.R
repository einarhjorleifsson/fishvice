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
                    high = ubnd / scale,
                    variable = variable) %>%
      dplyr::select(variable, year, est, low, high)
  }

  dplyr::bind_rows(FLCore::catch(fl) %>% lh("catch", scale = scale),
                   FLCore::rec(fl)   %>% lh("rec", scale = scale),
                   FLCore::ssb(fl)   %>% lh("ssb", scale = scale),
                   FLCore::tsb(fl)   %>% lh("tsb", scale = scale),
                   FLCore::fbar(fl)  %>% lh("fbar"))

}

fls_rbya <- function(fl, stk, des) {

  if(class(fl)[[1]] != "FLSAM")
    stop(paste0('Expected object class "FLSAM"\n',
                'Object passed is of class "', class(fl)), '"')

  if(missing(stk)) stk <- FLCore::name(fl)
  if(missing(des)) des <- FLCore::desc(fl)

  fl %>%
    FLCore:::as.data.frame() %>%
    tidyr::as_tibble() %>%
    dplyr::select(year, age, var = slot, val = data) %>%
    dplyr::mutate(var = dplyr::case_when(var == "stock.n" ~ "n",
                                         var == "harvest" ~ "f")) %>%
    tidyr::spread(var, val) %>%
    dplyr::mutate(stk = stk,
                  des = des)

}

fls_opr <- function(fl, lgs = TRUE, scale = 1) {

  if(class(fl)[[1]] != "FLSAM")
    stop(paste0('Expected object class "FLSAM"\n',
                'Object passed is of class "', class(fl)), '"')

  FLCore::residuals(fl) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(fleet = ifelse(fleet == "catch unique", "catch", fleet)) %>%
    dplyr::rename(o = log.obs, p = log.mdl, r = std.res) %>%
    dplyr::mutate(year = year - 1 + range(fl)[[4]])

}



