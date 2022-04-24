# NOTE: HOSKI read_separ1 at the bottom of file
# TODO
#  * Get pM and pF for mup_rbya - ideally this should be returned by muppet in
#    resultsbyyearandage
#  * Implement HOSKI
#              Including retro
#  * Question what to do about ssbW vs sW, the is used in the sam returns
#    for SSB weights.



#' @title muppet_rbya
#'
#' @description reads muppet year-and-age input and output
#'
#' @param path A directory path
#' @param scale A scaler (default 1)
#' @param assyear Assessment year, if missing (default) will use the year after the last
#' catch at age input
#' @param run Name of the run, if missing (default) will use the directory name
#' @param wide A boolean indicating if returned table wide (default TRUE). If FALSE variable are
#' return within column 'var' and values in column 'val'.
#'
#' @return A tibble

mup_rbya <- function(path, scale = 1, assyear, run, wide = TRUE)  {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  if(!file.exists(file.path(path, "resultsbyyearandage"))) {
    stop(paste0("File: '", file.path(path, "resultsbyyearandage"), "' does not exist"))
  }

  if(missing(run)) run <- basename(path)

  # rbya ---
  rbya <-
    readr::read_tsv(file.path(path, "resultsbyyearandage"),
                    na = c("-1", "0"),
                    show_col_types = FALSE)
  if(missing(assyear)) {
    assyear <- rbya %>% dplyr::filter(!is.na(ObsCno)) %>% dplyr::pull(year) %>% max()
    assyear <- assyear + 1
  }

  if (ncol(rbya) != 19) {
    rbya$pU2 <- NA
    rbya$oU2 <- NA
    rbya$rU2 <- NA
  }

  rbya <-
    rbya %>%
    dplyr::select(year, age,
                  n = N,
                  f = F,
                  oC = ObsCno,
                  pC = CalcCno,
                  rC = CatchDiff,
                  cW = CatchWeights,
                  ssbW = SSBWeights,
                  sW = StockWeights,
                  oU1 = ObsSurveyNr1,
                  pU1 = ObsSurveyNr2,
                  rU1 = SurveyResiduals1,
                  oU2 = ObsSurveyNr2,
                  pU2 = CalcSurveyNr2,
                  rU2 = SurveyResiduals2,
                  m = M,
                  z = Z)  %>%
    dplyr::mutate(oC = oC / scale,
                  pC = pC  / scale,
                  cW = cW / scale,
                  sW = sW / scale,
                  ssbW = ssbW / scale,
                  n = n / scale,
                  run = run,
                  model = "mup",
                  assyear = assyear,
                  yc = year - age)

  if(!wide) {
    rbya <-
      rbya %>%
      dplyr::select(-c(pC, rC, oU1, oU2, pU1, pU2, rU1, rU2)) %>%
      tidyr::gather(var, val, -c(year, age, run, model, assyear, yc)) %>%
      dplyr::select(year, age, var, val, dplyr::everything())
  }

  return(rbya)

}

mup_opr <- function(path, run, log = TRUE, sur = c("smb", "smh")) {
  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }
  d <- mup_rbya(path, run = run)
  d <-
    dplyr::bind_rows(
      d %>% dplyr::select(year, age, o =  oC, p =  pC, r =  rC) %>% dplyr::mutate(var = "catch"),
      d %>% dplyr::select(year, age, o = oU1, p = pU1, r = rU1) %>% dplyr::mutate(var = sur[1]),
      d %>% dplyr::select(year, age, o = oU2, p = pU2, r = rU2) %>% dplyr::mutate(var = sur[2])
    ) %>%
    dplyr::mutate(o = log(o),
                  p = log(p),
                  run = d$run[1])
  return(d)
}



mup_rby <- function(path, scale = 1, assyear, run) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  if(!file.exists(file.path(path, "resultsbyyear"))) {
    stop(paste0("File: '", file.path(path, "resultsbyyear"), "' does not exist"))
  }

  if(missing(run)) run <- basename(path)

  rby <- readr::read_tsv(file.path(path, "resultsbyyear"),
                         na = c("-1", "0"),
                         show_col_types = FALSE)
  nfleets <- (ncol(rby) - 14) / 2
  fleetnames <- as.character(1:nfleets)
  txty <- paste(c("pU","oU"),c(matrix(fleetnames,2,length(fleetnames),byrow=T)),sep="")
  txtya <- paste(c("pU","oU","rU"),c(matrix(fleetnames,3,length(fleetnames),byrow=T)),sep="")
  txta <- paste(c("cvU","qU","pU"),c(matrix(fleetnames,3,length(fleetnames),byrow=T)),sep="")

  names(rby)[15:ncol(rby)] <- txty

  rby <-
    rby %>%
    dplyr::rename(r = Recruitment,
                  bio = RefBio2,
                  ssb = Spawningstock,
                  fbar = RefF,
                  pY = CalcCatchIn1000tons,
                  oY = CatchIn1000tons,
                  #oU1 = ObsSurveyBiomass1,
                  #pU1 = CalcSurveyBiomass1,
                  #oU2 = ObsSurveyBiomass2,
                  #pU2 = CalcSurveyBiomass2,
                  bio1 = RefBio1,
                  bio2 = CbioR,
                  eggp = Eggproduction) %>%
    dplyr::mutate(y = ifelse(is.na(oY), pY, oY),
                  hr_old = y/bio,
                  hr = (1/3 * y + 3/4 * dplyr::lead(y)) / bio,
                  #hr1 = y / bio1,
                  #hr2 = y / bio2,
                  r = r / scale) %>%
    dplyr::select(year:fbar, hr, pY, oY, dplyr::everything()) %>%
    dplyr::select(-y)

  if(missing(assyear)) {
    assyear <- rby %>% dplyr::filter(!is.na(oY)) %>% dplyr::pull(year) %>% max()
    assyear <- assyear + 1
  }

  rby <-
    rby %>%
    #select(year, bio, ssb, r, hr, fbar, oY)
    dplyr::mutate(run = run,
                  model = "mup",
                  assyear = assyear)

  return(rby)
}

mup_rba <- function(path, run) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  if(!file.exists(file.path(path, "resultsbyage"))) {
    stop(paste0("File: '", file.path(path, "resultsbyage"), "' does not exist"))
  }

  if(missing(run)) run <- basename(path)

  rba <-
    readr::read_tsv(file.path(path, "resultsbyage"),
                    na = c("-1", "0"),
                    show_col_types = FALSE)

  n.fleets <- (ncol(rba) - 4) / 3

  cn <- c("age", "sel", "psel", "cvC", paste0(c("cvU", "qU", "pU"), rep(1:n.fleets, each = 3)))
  colnames(rba) <- cn

  rba <-
    dplyr::bind_cols(rba %>% dplyr::select(1, 4:ncol(rba)),
                     rba %>% dplyr::select(2:3)) %>%
    dplyr::mutate(run = run,
                  model = "mup")

  return(rba)
}

mup_std <- function(path) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  if(file.exists(paste0(path, "/muppet.par"))) {
    d <-
      utils::read.table(paste0(path, "/muppet.std"), header = TRUE) %>%
      tibble::as_tibble()
    return(d)
  }

  # NOTE: function returns NULL if muppet.par does not exist
}


mup_par <- function(path) {

  if(file.exists(paste0(path, "/muppet.par"))) {
    fil <- paste0(path, "/muppet.par")
  } else {
  }

  x <- readr::read_lines(fil)
  head <- x[1]
  head <-   stringr::str_split(head, " ")[[1]]
  head <- head[c(6, 11, 17)] %>% stringr::str_trim() %>% as.numeric()
  names(head) <- c("npar", "objective", "max_gradient")

  idx <- grep("#", x)
  N <- length(idx)
  res <- list()
  for(i in 1:N) {
    if(i == 1) {
      res[[i]] <- head
    } else {
      if(i < N) {
        i1 <- idx[i] + 1
        i2 <- idx[i + 1] - 1
        res[[i]] <-
          x[i1:i2] %>%
          stringr::str_trim() %>%
          stringr::str_split(" ", simplify = TRUE)
        nr <- nrow(res[[i]])
        if(nr > 1) {
          res[[i]] <-
            res[[i]] %>%
            as.numeric() %>%
            matrix(nrow = nr)
        } else {
          res[[i]] <-
            res[[i]] %>%
            as.numeric()
        }

      } else {
        res[[i]] <- x[N] %>% as.numeric()
      }
    }
  }
  names(res) <-
    c("obj", x[idx[-1]]) %>%
    stringr::str_replace_all("#", "") %>%
    stringr::str_replace(":", "") %>%
    stringr::str_replace("\\[", "") %>%
    stringr::str_replace("\\]", "") %>%
    stringr::str_trim()

  return(res)
}

#' @title muppetrbx
#'
#' @description reads muppet results
#'
#' @param path A directory path
#' @param scale A scaler (default 1)
#' @param assyear Assessment year, if missing (default) will use the year after the last
#' catch at age input
#' @param run Name of the run, if missing (default) will use the directory name
#' @param wide A boolean indicating if returned table wide (default TRUE). If FALSE variable are
#' return within column 'var' and values in column 'val'.
#'
#' @return A tibble
#'
#' @export
#'
#' @examples
#' rbx <- mup_rbx(path = system.file("mup/smx", package = "fishvice"), scale = 1000)
mup_rbx <- function(path, scale = 1, assyear, run, wide = TRUE) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  list(rby  = mup_rby(path, scale, assyear, run),
       rbya = mup_rbya(path, scale, assyear, run, wide),
       rba  = mup_rba(path, run),
       opr  = mup_opr(path, run),
       std  = mup_std(path),
       par  = mup_par(path))

}


# HOSKI ------------------------------------------------------------------------
# Hér sérðu dæmi um nýja read_separ (read_separ1) í bili
# notkun á því og útkomu.
# res <- read_separ1(".",".",fleetnames=c("3","1","2","4"),assYear=year+1)
#
# Floti 1 er alltaf mars survey , 2 er haustrall 3 er marssurvey 1-2 og 4 haustrall12 það er út af þessu logq1 dæmi
#
# Við höldum alltaf þessum nöfnum þ.a í haustrallkeyrslu er bara oU2 og pU2 ekki oU1 og pU1.
#
# Ef vilja hafa þetta öðruvísi er defaultið að númera flotanna í vaxandi röð.
# read_separ1 <- function (path, run, rName = NA, mName = NA, calcSurBio = F,
#                          ggFactor = T, Scale = 1000, assYear = NA, retroY = NA,fleetnames)
# {
#   if(file.exists("muppet.par")){ # get AIC could look for as.numeric=T
#     dat <- scan("muppet.par",what=character(),sep=" ",quiet=T)[1:12]
#     aicinfo <- as.numeric(c(dat[6],dat[11]))
#     names(aicinfo) <- c("npar","objective")
#
#     dat <- scan("muppet.par",what=character(),sep="\n",quiet=T)
#     i <- grep("# surveybiopow",dat)
#     surveybiopow <- as.numeric(dat[i+1])
#   }
#
#
#
#   if(missing(fleetnames)){ # to have some default
#     # Test number of columns
#     if (is.na(retroY))
#       rby <- read.table(paste(path, run, "resultsbyyear", sep = "/"),
#                         header = T, na.strings = c("-1", "0"))
#     if (!is.na(retroY))
#       rby <- read.table(paste(paste(path, run, "resultsbyyear",
#                                     sep = "/"), retroY, sep = ""),
#                         header = T, na.strings = c("-1", "0"))
#     nfleets <- (ncol(rby)-14)/2
#     fleetnames <- as.character(1:nfleets)
#   }
#   txty <- paste(c("oU","pU"),c(matrix(fleetnames,2,length(fleetnames),byrow=T)),sep="")
#   cnRby <- c("year", "r", "n3", "n6", "bioF", "bio", "bio1",
#              "ssb", "ssb2", "fbar", "hr", "oY", "pY",txty,"run", "model")
#   txtya <- paste(c("oU","pU","rU"),c(matrix(fleetnames,3,length(fleetnames),byrow=T)),sep="")
#   cnRbya <- c("year", "age", "oC", "cW", "sW", "ssbW", "mat",
#               "n", "z", "f", "m", "pC", "rC",txtya)
#   txta<- paste(c("cvU","qU","pU"),c(matrix(fleetnames,3,length(fleetnames),byrow=T)),sep="")
#
#   cnRba <- c("age", "sel", "pSel", "sigma", txta, "run", "model")
#   if (is.na(retroY))
#     rby <- read.table(paste(path, run, "resultsbyyear", sep = "/"),
#                       header = T, na.strings = c("-1", "0"))
#   if (!is.na(retroY)) {
#     rby <- read.table(paste(paste(path, run, "resultsbyyear",
#                                   sep = "/"), retroY, sep = ""), header = T, na.strings = c("-1",
#                                                                                             "0"))
#   }
#   n <- nrow(rby)
#   if (ncol(rby) != 18) {
#     rby$pU2 <- rep(NA, n)
#     rby$oU2 <- rep(NA, n)
#   }
#   names(rby) <- c("year", "fbar", "pY", "oY", "ssb", "ssb2",
#                   "bioF", "bio1", "bio", "preR", "r", "n1", "n3", "n6",
#                   txty)
#   if (ggFactor)
#     rby$r <- rby$r * exp(-0.4)
#   rby$hr <- ifelse(!is.na(rby$oY), rby$oY, rby$pY)/rby$bio
#   rby$run <- rName
#   rby$model <- mName
#   rby <- rby[, cnRby]
#   rby$r <- rby$r/Scale
#   rby$n3 <- rby$n3/Scale
#   rby$n6 <- rby$n6/Scale
#   if (is.na(retroY))
#     rbya <- read.table(paste(path, run, "resultsbyyearandage",
#                              sep = "/"), header = T, na.strings = c("-1", "0"))
#   if (!is.na(retroY)) {
#     rbya <- read.table(paste(paste(path, run, "resultsbyyearandage",
#                                    sep = "/"), retroY, sep = ""), header = T, na.strings = c("-1",
#                                                                                              "0"))
#   }
#   n <- nrow(rby)
#   names(rbya) <- c("year", "age", "n", "z", "sW", "m", "f",
#                    "pC", "cW", "ssbW", "mat", "oC", "rC",txtya)
#   if (ggFactor)
#     rbya$n <- ifelse(rbya$age %in% 1, rbya$n * exp(-0.4),
#                      rbya$n)
#   if (ggFactor)
#     rbya$n <- ifelse(rbya$age %in% 2, rbya$n * exp(-0.2),
#                      rbya$n)
#   rbya <- rbya[, cnRbya]
#   rbya$run <- rName
#   rbya$model <- mName
#   rbya$oC <- rbya$oC/Scale
#   rbya$cW <- rbya$cW/Scale
#   rbya$sW <- rbya$sW/Scale
#   rbya$n <- rbya$n/Scale
#   rbya$pC <- rbya$pC/Scale
#   if (is.na(retroY))
#     rba <- read.table(paste(path, run, "resultsbyage", sep = "/"),
#                       header = T, na.strings = c("-1", "0"))
#   if (!is.na(retroY)) {
#     rba <- read.table(paste(paste(path, run, "resultsbyage",
#                                   sep = "/"), retroY, sep = ""), header = T, na.strings = c("-1",
#                                                                                             "0"))
#   }
#   n <- nrow(rba)
#   names(rba) <- c("age", "sel", "pSel", "sigma", txta)
#   rba$run <- rName
#   rba$model <- mName
#   rba <- rba[, cnRba]
#   if (!is.na(retroY)) {
#     print(retroY)
#     rby$assYear <- as.numeric(retroY) + 1
#     rbya$assYear <- as.numeric(retroY) + 1
#     rba$assYear <- as.numeric(retroY) + 1
#   }
#   else {
#     rby$assYear <- assYear
#     rbya$assYear <- assYear
#     rba$assYear <- assYear
#   }
#   if(exists("surveybiopow")){
#     names(surveybiopow) <- fleetnames
#     return(list(rby = rby, rbya = rbya, rba = rba,aicinfo=aicinfo,surveybiopow=surveybiopow))
#   }
#   else
#     return(list(rby = rby, rbya = rbya, rba = rba)) # no muppet.par
# }
#
# # retro example ----------------------------------------------------------------
# source("readSepar.r")
# library(stringr)
# library(tidyverse)
#
# Changepinfile <- function(file="muppet.par",txt = c("# lnRecr:","# lnEffort:"),outputfile="muppet.pin") {
#   dat <- scan(file,what=character(),sep="\n",quiet=TRUE)
#   for(k in 1:length(txt)){
#     j <- grep(txt[k],dat)
#     if(length(j) > 0) {
#       k1 <- unlist(str_locate_all(dat[j+1]," "))
#       dat[j+1]  <- substring(dat[j+1],1,k1[length(k1)]-1)
#     }
#   }
#   write.table(dat,file=outputfile,row.names=FALSE,col.names=FALSE,sep="\n",quote=F)
# }
#
# Replace <- function(txt,parameter,pattern){
#   if(!missing(parameter)){
#     i <- grep(pattern,txt)
#     if(!any(i)){
#       print(paste("   ",pattern,"   ","does not exist"))
#       break()
#     }
#     txt[i] <- paste(as.character(parameter),"\t",pattern)
#   }
#   return(txt)
# }
# rby <- rbya <- rba <- aicinfo <- surveybiopow <-  list()
# PIN <- TRUE
#
# inputfile <- "icecod.dat.opt.final"
# for(year in c(2019:2001)){
#   print(year)
#   assyear <- year+1
#   txt <- readLines(inputfile)
#   txt <- Replace(txt,year,'# Last opt year')
#   txt <- Replace(txt,min(c(year+2,2019)),'# Last data year')
#
#   if(PIN && (year != 2019))Changepinfile("muppet.par",txt = c("# lnRecr:","# lnEffort:"),outputfile="muppet.pin")
#   write.table(txt,file="icecod.dat.opt",sep="\n",row.names=F,quote=F,col.names=F)
#   system("muppet -nox -ind icecod.dat.opt > /dev/null")
#   res <- read_separ1(".",".",fleetnames=c("3","1","2","4"),assYear=year+1)
#   rby[[as.character(assyear)]] <- res$rby
#   rbya[[as.character(assyear)]] <- res$rbya
#   rba[[as.character(assyear)]] <- res$rba
#   aicinfo[[as.character(assyear)]] <- res$aicinfo
#   surveybiopow[[as.character(assyear)]] <- res$surveybiopow
#
#
#   # Those mv are really not needed but we do at least to remove the files.
#   system(paste("mv resultsbyyear tmpresults/resultsbyyear",year,sep=""))
#   system(paste("mv resultsbyyearandage tmpresults/resultsbyyearandage",year,sep=""))
#   system(paste("mv resultsbyage tmpresults/resultsbyage",year,sep=""))
# }
#
# rby <- bind_rows(rby)
# rbya <- bind_rows(rbya)
# rba <- bind_rows(rba)
# aicinfo <- bind_rows(aicinfo)
# aicinfo$assYear <- unique(rby$assYear)
# surveybiopow <- bind_rows(surveybiopow)
# surveybiopow$assYear <- unique(rby$assYear)
#
# save(list=c("rby","rbya","rba","aicinfo","surveybiopow"),file="retro.rdata")
#

