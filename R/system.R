#' @title Find text in file
#'
#' @description XXX
#'
#' @export
#'
#' @param string Pattern to search
#' @param dir Directory to search
sys_find_text <- function(string, dir) {

  if(missing(dir)) dir <- "."
  cmd <- paste0('grep -rnw "',dir, '" -e ',string)
  system(cmd)
}
