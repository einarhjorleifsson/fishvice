# #' @title get_file
# #'
# #' @description Gets a file from the web.
# #'
# #' @param URL URL path
# #' @param File Name of the file
get_file <- function(URL,File)
{
  temporaryFile <- tempfile()
  utils::download.file(paste(URL,File,sep="/"),
                       destfile=temporaryFile,
                       method="curl",quiet = T)
  return(temporaryFile)
}


