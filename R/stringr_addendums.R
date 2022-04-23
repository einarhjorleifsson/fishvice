#' @title Strip all text from a string
#'
#' @description Function strips all alphabetic characters (a-z and A-Z) from a string
#'
#'
#' @param string A character vector

str_strip_text <- function(string) {
  as.numeric(gsub("[^\\d]+", "", string, perl=TRUE))
}

#' @title Strip all non digits from a string
#'
#' @description Function strips all alphabetic and "special" characters from a string.
#' Ergo, only digits 0-9 and the decimal (".") is returned.
#'
#'
#' @param string A character vector
#' @param out.numeric Boolean, if TRUE (default) string coerced to numeric.

str_strip_nondigits <- function(string, out.numeric = TRUE) {
 x <- gsub("[^0-9.]", "", string, perl = TRUE)
 if(out.numeric) x <- as.numeric(x)
 return(x)
}

#' @title Trim 'tabs' from start and end of string
#'
#' @description Function is inspired by str_trim function in the stringr package.
#' The str_trim function is for trimming whitespace, here tabs are trimmed.
#'
#'
#' @param string input character vector
#' @param side side on which character string is removed (left, right or both)

str_trim_tab <- function(string,side='both') {
  #string <- stringr:::check_string(string)
  stopifnot(length(side) == 1)
  side <- match.arg(side, c("left", "right", "both"))
  pattern <- switch(side, left = "^\\t+", right = "\\t+$",
                    both = "^\\t+|\\t+$")
  stringr::str_replace_all(string, pattern, "")
}

#' @title Trim 'commas' from start and end of string
#'
#' @description Function is enspired by str_trim function in the stringr package.
#' @param string input character vector
#' @param side side on which character string is removed (left, right or both)
#'

str_trim_commas <- function(string,side='both') {
  #string <- stringr:::check_string(string)
  stopifnot(length(side) == 1)
  side <- match.arg(side, c("left", "right", "both"))
  pattern <- switch(side, left = "^,+", right = ",+$",
                    both = "^,+|,t+$")
  stringr::str_replace_all(string, pattern, "")
}
