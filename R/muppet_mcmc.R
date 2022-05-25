#' read mcmc file
#'
#' @param path The directory containing the mcmc files
#' @param file A character vector containing the file name. If missing (DEFAULT)
#' reads in all year-results
#' @param std A boolean, if TRUE (default) returns niceer var names
#'
#' @return a tibble three variables: year, val, var and iter
#' @export
#'
mup_mcmc <- function(path, file, std = TRUE) {

  if(!missing(file)) {
    if(stringr::str_detect(file, ".mcmc")) {
      path <- paste0(path, "/", file, ".mcmc")
    } else {
      path <- paste0(path, "/", file)
    }
  }  else {
    path <- fs::dir_ls(path, glob = "*.mcmc")
    path <- path[-grep("all.mcmc", path)]
    path <- path[-grep("parameter.mcmc", path)]
  }

  res <-
    purrr::map(path, readr::read_delim, delim = " ", col_types = "n") %>%
    purrr::map(mutate, iter = 1:dplyr::n())
  d <-
    dplyr::bind_rows(res) %>%
    tidyr::gather(var, val, -iter) %>%
    tidyr::separate(var, into = c("var", "year"), sep = "\\.", convert = TRUE) %>%
    dplyr::select(year, val, var, iter)

  if(std) {
    d <-
      d %>%
      dplyr::mutate(var = dplyr::case_when(var == "AssessmentErr" ~ "asserr",
                                           var == "CalcCatchIn1000tons" ~ "oY",
                                           var == "FishingYearCatch" ~ "oY2",
                                           var == "HCRrefbio" ~ "bio",
                                           var == "RefBio1" ~ "bio1",
                                           var == "RefBio2" ~ "bio2",
                                           var == "RefF" ~ "fbar",
                                           var == "Spawningstock" ~ "ssb",
                                           TRUE ~ var))
  }

  return(d)

}
