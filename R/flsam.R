#' Title
#'
#' @param fls An FLSAM objecct
#' @param scale xxx
#'
#' @return A tibble
#'
flsam_rby <- function(fls, scale = 1) {

  lh <- function(x, variable, scale = 1) {
    x %>%
      tibble::as_tibble() %>%
      #tibble::remove_rownames() %>%
      #dplyr::mutate(year = fls@range["minyear"]:fls@range["maxyear"] ) %>%
      dplyr::mutate(variable = variable,
                    est = value / scale,
                    low = lbnd / scale,
                    high = ubnd / scale) %>%
      dplyr::select(variable, year, est, low, high)
    }

  dplyr::bind_rows(FLCore::catch(fls) %>%  lh(., "est. catch", scale = scale),
                   FLCore::rec(fls)   %>% lh("rec", scale = scale),
                   FLCore::ssb(fls)   %>% lh("ssb", scale = scale),
                   FLCore::tsb(fls)   %>% lh("tsb", scale = scale),
                   FLCore::fbar(fls)  %>% lh("fbar"))

}
