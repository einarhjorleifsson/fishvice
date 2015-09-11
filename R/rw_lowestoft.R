#' @title read_lowestoft_file
#'
#' @description The code is inpired by the
#'  \href{https://github.com/flr/FLCore/blob/master/R/io.VPAsuite.R}{FLCore::readVPAFile} function.
#' The difference is that it is not dependent on the FLCore, including the S4-methods.
#'
#' @export
#'
#' @param file name of file, normally the index file name
#' @param format output format, "matrix","wide","long" (default)
#' @param value.name Name of value, only relevant for format="long"
#' @param sep the separator, default is ""
#' @param quiet boolean, default is TRUE

read_lowestoft2 <- function(file, format="long", value.name="x",sep = "", quiet = TRUE) {

  if (!file.exists(file))
    {
    if(quiet==TRUE) message()
    if(quiet!=TRUE) message(paste("VPA index file", file, "does not exist"))
    return(NULL)
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
  return(dplyr::as_data_frame(res))
  }
}

#' @title read_lowestoft
#'
#' @description Modified \href{https://github.com/flr/FLCore/blob/master/R/io.VPAsuite.R}{FLCore::readVPA}
#' function in the FLCore-package, except here it only returns a list or a data.frame
#'
#' @export
#'
#' @param file name of file, normally the index file name
#' @param sep the separator, default is ""
#' @param format character vector specifying output format: "long" (default),
#' "wide" or "list"
#' but can also use "list" which returns a list of matrices.
#' @param quiet boolean, default is TRUE


read_lowestoft <- function(file, sep = "", format = "long", quiet=TRUE) {
  if (!file.exists(file)){
    if(quiet!=TRUE) message(paste("VPA index file", file, "does not exist"))
    return(NULL)
  }
  dir <- dirname(file)
  files. <- scan(file, what = "character", skip = 2, sep = sep, quiet=quiet)
  for(i in seq(length(files.)))
    if (!grepl(dir,files.[i]))
      files.[i] <- file.path(dir, files.[i], fsep = .Platform$file.sep)
  range1 <- scan(files.[1], skip = 2, nlines = 1, sep = sep, quiet=quiet)
  range2 <- scan(files.[1], skip = 3, nlines = 1, sep = sep, quiet=quiet)
  range <- c(range1[1:2],range2[1:2])
  ages <- range[3:4]
  yrs <- range[1:2]
  #FLStock. <- FLStock(catch.n=FLQuant(NA, dimnames = list(age = ages[1]:ages[2], year = yrs[1]:yrs[2], unit = "unique", season = "all", area = "unique")))
  res <- list()
  for (i in files.) {
    if (!file.exists(i)){
      if(quiet != TRUE) cat("File ", i, "does not exist", "\n")
    }
    if (file.exists(i)) {
      a. <- read_lowestoft2(i, format = "matrix", sep=sep, quiet=quiet)
      switch(as.character(scan(i, skip = 1, nlines = 1, sep = sep, comment.char='#', quiet=TRUE)[2]),
             "1" = res$landings <-a.,
             "2" = res$landings.n <-a.,
             "3" = res$landings.wt <-a.,
             "4" = res$stock.wt <-a.,
             "5" = res$m <-a.,
             "6" = res$mat <-a.,
             "7" = res$harvest.spwn <-a.,
             "8" = res$m.spwn <-a.,
             "21"= res$discards <-a.,
             "22"= res$discards.n <-a.,
             "23"= res$discards.wt <-a.,
             "24"= res$catch <-a.,
             "25"= res$catch.n <-a.,
             "26"= res$catch.wt <-a.,
             "27"= res$harvest <-a.,
             "28"= res$stock.n <-a. )
    }
  }

  # change names to rvk-standard
  # pending

  res$range <- c(min = ages[1], max = ages[2],
                 plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
  res$desc <- paste("Imported from a VPA file (",
                    file, "). ", date(), sep="")
  res$name <- scan(file, nlines = 1, what = character(0),
                   sep = "\n", quiet=TRUE)


  # Landings
  bya <- reshape2::melt(res$landings.n,value.name = "oL")
  # Catch
  if(is.null(res$catch.n)) {
    bya$oC <- bya$oL
  } else {
    x <- reshape2::melt(res$catch.n,value.name="oC")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # Discards
  if(is.null(res$discards.n)) {
    bya$oD <- 0
  } else {
    x <- reshape2::melt(res$discards.n,value.name="oD")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # Landings weight
  x <- reshape2::melt(res$landings.wt,value.name="lW")
  bya <- dplyr::full_join(bya,x,by=c("year","age"))
  # Catch weigths
  if(is.null(res$catch.wt)) {
    bya$cW <- bya$lW
  } else {
    x <- reshape2::melt(res$catch.wt,value.name="cW")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # Discard weights
  if(is.null(res$discards.wt)) {
    bya$dW <- 0
  } else {
    x <- reshape2::melt(res$discards.wt,value.name="dW")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # SSB weights
  if(is.null(res$stock.wt)) {
    bya$ssbW <- bya$catch.wt
  } else {
    x <- reshape2::melt(res$stock.wt,value.name="ssbW")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # Maturity
  if(is.null(res$mat)) {
    bya$mat <- NA
  } else {
    x <- reshape2::melt(res$mat,value.name="mat")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # M
  if(is.null(res$m)) {
    bya$m <- NA
  } else {
    x <- reshape2::melt(res$m,value.name="m")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # pF
  if(is.null(res$harvest.spwn)) {
    bya$pF <- NA
  } else {
    x <- reshape2::melt(res$harvest.spwn,value.name="pF")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # pM
  if(is.null(res$m.spwn)) {
    bya$pM <- NA
  } else {
    x <- reshape2::melt(res$m.spwn,value.name="pM")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }
  # fishing mortality
  # Problem here is that res$harvest.spwn gets tested
  #if(!is.null(res$harvest)) {
  #  x <- reshape2::melt(res$harvest,value.name="f")
  #  bya <- plyr::join(bya,x,by=c("year","age"))
  #}
  # stock in numbers
  if(!is.null(res$stock.n)) {
    x <- reshape2::melt(res$stock.n,value.name="n")
    bya <- dplyr::full_join(bya,x,by=c("year","age"))
  }

  if(format == "long") {
    res <- bya[order(bya$year),] %>%
      dplyr::as_data_frame()
  }

  return(res)
}

#' @title Read in Lowestoft-format survey data
#'
#' @export
#'
#' @param filename The name of the file to read in
#' @param format Character, if "data.frame" (default) returns a data.frame
#' otherwise a list.
read_lowestoft_survey <- function(filename, format="data.frame")
{
  n <- scan(filename, skip = 1, nlines = 1, quiet = TRUE) - 100
  idx <- vector("list", length = n)
  start <- 3
  for (k in 1:n)
  {
    idx[[k]]$name <- paste(scan(filename, skip = start - 1, nlines = 1,
                                what = character(0), quiet = TRUE), collapse = " ")
    temp <- scan(filename, skip = start, nlines = 1, quiet = TRUE)
    idx[[k]]$y1 <- temp[1]
    idx[[k]]$y2 <- temp[2]
    idx[[k]]$ny <- temp[2] - temp[1] + 1
    temp <- scan(filename, skip = start + 1, nlines = 1, quiet = TRUE)
    idx[[k]]$rho <- 0.5 * (temp[4] + temp[3])
    temp <- scan(filename, skip = start + 2, nlines = 1, quiet = TRUE)
    idx[[k]]$a1 <- temp[1]
    idx[[k]]$a2 <- temp[2]
    idx[[k]]$na <- temp[2] - temp[1] + 1
    idx[[k]]$tab <- read.table(filename, skip = start + 3, nrows = idx[[k]]$ny)
    temp <- idx[[k]]$tab[,2:(idx[[k]]$na + 1)]
    effort <- idx[[k]]$tab[,1]
    idx[[k]]$tab <- data.frame(temp / effort)
    names(idx[[k]]$tab) <- idx[[k]]$a1:idx[[k]]$a2
    rownames(idx[[k]]$tab) <- idx[[k]]$y1:idx[[k]]$y2
    start <- start + 4 + idx[[k]]$ny
  }

  res <- list(n = n, idx = idx)

  if(format == "data.frame") {
    for (i in 1:n) {
      x <- res$idx[[i]]
      x1 <- x$tab
      x1$year <- as.integer(rownames(x1))
      x1 <- reshape2::melt(x1,"year", variable.name = "age")
      x1$sur <- x$name
      if(i == 1) {
        tmp <- x1
      } else {
        tmp <- rbind(tmp, x1)
      }
    }
    res <- tmp
    res$age <- as.integer(as.character(res$age))
    res <- dplyr::as_data_frame(res)

  }

  return(res)
}



