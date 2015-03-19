#' @title get_file
#'
#'
#' @param URL URL path
#' @param File Name of the file
get_file <- function(URL,File)
{
  temporaryFile <- tempfile()
  download.file(paste(URL,File,sep="/"),
                destfile=temporaryFile,
                method="curl",quiet = T)
  return(temporaryFile)
}
