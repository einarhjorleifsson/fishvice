#' @title read_lowestoft_file
#'
#' @description The code is inpired by the
#'  \href{https://github.com/flr/FLCore/blob/master/R/io.VPAsuite.R}{FLCore::readVPAFile} function.
#' The difference is that it is not dependent on the FLCore, including the S4-methods.
#'
#' @param file name of file, normally the index file name
#' @param format output format, "matrix","wide","long"
#' @param value.name Name of value, only relevant for format="long"
#' @param sep the separator, default is ""
#' @param quiet boolean, default is TRUE


read_lowestoft <- function(file, format="matrix", value.name="x",sep = "", quiet = TRUE) {

  if (!file.exists(file))
    {
    if(quiet==TRUE) stop()
    if(quiet!=TRUE) stop(paste("VPA index file", file, "does not exist"))
  }

  switch (as.character(file.access(file)),
          "0" = info <- read.table(file, colClasses = "character",
                                   header = FALSE, fill = TRUE, skip = 1,
                                   nrows = 4, sep = sep, comment.char='#'),
          "-1" = info <- matrix(rep("0", 8), nrow = 4, ncol = 2))
  misc <- info[1, 1]
  type <- info[1, 2]
  dfor <- info[4, 1]
  # Switch for file type (dfor; e.g. matrix, scalar, vector)
  switch(misc,
         "1" = {range <- scan(file,
                              skip = 2,
                              nlines = 2,
                              sep = sep,
                              comment.char='#',
                              quiet=quiet)
         ages <- range[3:4]
         nages <- ages[2] - ages[1] + 1
         yrs <- range[1:2]
         nyrs <- yrs[2] - yrs[1] + 1
         dms <- list(year=as.character(yrs[1]:yrs[2]),age=as.character(ages[1]:ages[2]))
         switch(dfor,
                "1" = res <- matrix(scan(file,
                                        skip=5,
                                        comment.char="#",
                                        quiet=quiet),
                                   ncol=nages,
                                   nrow=nyrs,
                                   byrow=T,
                                   dimnames= dms)[1:nyrs, 1:nages],
                "2" = res <- matrix(rep(scan(file,
                                            skip = 5,
                                            sep = sep,
                                            comment.char='#',
                                            quiet=quiet)[1:nages], nyrs),
                                   ncol = nages,
                                   nrow = nyrs,
                                   byrow=T,
                                   dimnames = dms),
                "3" = res <- matrix(rep(scan(file,
                                            skip = 5,
                                            sep = sep,
                                            comment.char='#',
                                            quiet=quiet)[1], nyrs * nages),
                                   ncol = nages,
                                   nrow = nyrs,
                                   dimnames = dms),
                "5" = {
                  dms <- list(year=as.character(yrs[1]:yrs[2]))
                  res <- matrix(t(read.table(file = file,
                                            skip = 5,
                                            nrows = nyrs,
                                            sep = sep)[,1]),
                               ncol = 1,
                               nrow = nyrs,
                               dimnames = dms)
                })
         #needed to go from int to double
         #res <- as.numeric(res)
         },
         "0" = cat("Invalid file. Cannot read file:-", file, "\n"),
         if(quiet != TRUE) {
           cat("Tuning file", file, "not read", "\n")
           return(invisible(NULL))
           }
         ) # end switch

  if(format=="matrix") return(res)

  res <- as.data.frame(res)
  res$year <- as.integer(rownames(res))
  rownames(res) <- NULL
  res <- res[,c(ncol(res),1:(ncol(res)-1))]

  if(format=="wide") {
    return(res)
  } else { # format=="long"
    res <- reshape2::melt(res,"year",variable.name="age",value.name = value.name)
    res$age <- as.integer(as.character(res$age))
    res[,3] <- as.numeric(res[,3])
  return(res)
  }
}

