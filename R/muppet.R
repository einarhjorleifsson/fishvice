# TODO
#  * Get pM and pF for mup_rbya - ideally this should be returned by muppet in
#    resultsbyyearandage
#  * mup_rbx - RETRO
#  * Question what to do about ssbW vs sW, the latter is what sam returns
#    for SSB weights.
# check if one can not do something like this, or better still include
#  this type of thing in a xxxx.sh file within the package
# system("nohup ./muppet -nox -ind icecod.dat.opt -mcmc 500000 -mcscale -mcsave 1000 &")



# #' @title muppet_rbya
# #'
# #' @description reads muppet year-and-age input and output
# #'
# #' @param path A directory path
# #' @param scale A scaler (default 1)
# #' @param fleets Name of fleets, e.g. if only fall survey tuning, use fleets = "2".
#      If missing (default) then use sequential numbers.
# #' @param assyear Assessment year, if missing (default) will use the year after the last
# #' catch at age input
# #' @param run Name of the run, if missing (default) will use the directory name
# #' @param wide A boolean indicating if returned table wide (default TRUE). If FALSE variable are
# #' return within column 'var' and values in column 'val' (not active)
# #'
# #' @return A tibble

mup_rbya <- function(path, scale = 1, fleets, assyear, run, wide = TRUE)  {

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
                    show_col_types = FALSE) %>%
    dplyr::rename(oC = ObsCno,
                  pC = CalcCno,
                  rC = CatchDiff)
  # check length of fleets vs nfleets
  if(!missing(fleets)) {
    nfleets <- (ncol(rbya) - 13) / 3
    if(nfleets != length(fleets)) {
      stop(paste0("Named fleets (", paste(fleets, collapse = " ") , ") not the same as number of fleets (", nfleets, ")" ))
    }
  }
  if(missing(fleets)) {
    nfleets <- (ncol(rbya) - 13) / 3
    fleets <- as.character(1:nfleets)
  }
  txty <- paste(c("pU","oU", "rU"),c(matrix(fleets,3,length(fleets),byrow=T)),sep="")
  names(rbya)[14:ncol(rbya)] <- txty



  if(missing(assyear)) {
    assyear <- rbya %>% dplyr::filter(!is.na(oC)) %>% dplyr::pull(year) %>% max()
    assyear <- assyear + 1
  }

  #if (ncol(rbya) != 19) {
  #  rbya$pU2 <- NA
  #  rbya$oU2 <- NA
  #  rbya$rU2 <- NA
  #}

  rbya <-
    rbya %>%
    dplyr::select(year, age,
                  n = N,
                  f = F,
                  oC,
                  pC,
                  rC,
                  cW = CatchWeights,
                  ssbW = SSBWeights,
                  sW = StockWeights,
                  mat = StockMaturity,
                  m = M,
                  z = Z,
                  dplyr::everything()
    )  %>%
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

  # if(!wide) {
  #   rbya <-
  #     rbya %>%
  #     dplyr::select(-c(pC, rC, oU1, oU2, pU1, pU2, rU1, rU2)) %>%
  #     tidyr::gather(var, val, -c(year, age, run, model, assyear, yc)) %>%
  #     dplyr::select(year, age, var, val, dplyr::everything())
  # }

  return(rbya)

}


mup_opr <- function(path, scale = 1, fleets, assyear, run, log = TRUE) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  d <-
    mup_rbya(path, scale = scale, fleets = fleets, assyear = assyear, run = run) %>%
    dplyr::rename(.run = run)

  lh <- function(x, var, what) {
    x %>%
      dplyr::select(year, age, dplyr::starts_with(c(what)), .run, assyear) %>%
      tidyr::gather(fleet, {{ var }}, -c(year, age, .run, assyear)) %>%
      dplyr::mutate(fleet = stringr::str_sub(fleet, 2),
                    fleet = ifelse(fleet == "C", "catch", fleet))
  }
  d <-
    dplyr::full_join(d %>% lh(o, "o"),
                     d %>% lh(p, "p"),
                     by = c("year", "age", ".run", "assyear", "fleet")) %>%
    dplyr::full_join(d %>% lh(r, "r"),
                     by = c("year", "age", ".run", "assyear", "fleet"))

  d2 <-
    mup_rby(path, scale = scale, fleets = fleets, assyear = assyear, run = run) %>%
    dplyr::rename(.run = run)
  lh2 <- function(x, var, what) {
    x %>%
      dplyr::select(year, dplyr::starts_with(c(what)), .run, assyear) %>%
      tidyr::gather(fleet, {{ var }}, -c(year, .run, assyear)) %>%
      dplyr::mutate(fleet = stringr::str_sub(fleet, 2),
                    fleet = ifelse(fleet == "Y", "catch", fleet))
  }
  d2 <-
    dplyr::full_join(d2 %>% lh2(o, "o"),
                     d2 %>% lh2(p, "p"),
              by = c("year", ".run", "assyear", "fleet")) %>%
    dplyr::mutate(r = log(o/p))

  d <- dplyr::bind_rows(d, d2)


  if(log) {
    d <-
      d %>%
      dplyr::mutate(o = log(o),
                    p = log(p))
  }

  d <- d %>% dplyr::rename(run = .run)

  return(d)
}



mup_rby <- function(path, scale = 1, fleets, assyear, run) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  if(!file.exists(file.path(path, "resultsbyyear"))) {
    stop(paste0("File: '", file.path(path, "resultsbyyear"), "' does not exist"))
  }

  if(missing(run)) run <- basename(path)

  rby <- readr::read_tsv(file.path(path, "resultsbyyear"),
                         na = c("-1", "0"),
                         show_col_types = FALSE) %>%
    janitor::remove_empty(which = "cols")
  # check length of fleets vs nfleets
  if(!missing(fleets)) {
    nfleets <- (ncol(rby) - 14) / 2
    if(nfleets != length(fleets)) {
      stop(paste0("Named fleets (", paste(fleets, collapse = " ") , ") not the same as number of fleets (", nfleets, ")" ))
    }
  }
  if(missing(fleets)) {
    nfleets <- (ncol(rby) - 14) / 2
    fleets <- as.character(1:nfleets)
  }
  txty <- paste(c("pU","oU"),c(matrix(fleets,2,length(fleets),byrow=T)),sep="")
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
                  hr = y/bio,
                  hr2 = (1/3 * y + 3/4 * dplyr::lead(y)) / bio,
                  r = r / scale) %>%
    dplyr::select(year:fbar, hr, pY, oY, dplyr::everything()) %>%
    dplyr::select(-y)

  if(missing(assyear)) {
    assyear <- rby %>% dplyr::filter(!is.na(oY)) %>% dplyr::pull(year) %>% max()
    assyear <- assyear + 1
  }

  rby <-
    rby %>%
    dplyr::mutate(run = run,
                  model = "mup",
                  assyear = assyear)

  return(rby)
}

# note: assyear not extractable by default
mup_rba <- function(path, fleets, run) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  if(!file.exists(file.path(path, "resultsbyage"))) {
    stop(paste0("File: '", file.path(path, "resultsbyage"), "' does not exist"))
  }



  rba <-
    readr::read_tsv(file.path(path, "resultsbyage"),
                    na = c("-1", "0"),
                    show_col_types = FALSE)

  # check length of fleets vs nfleets
  if(!missing(fleets)) {
    nfleets <- (ncol(rba) - 4) / 3
    if(nfleets != length(fleets)) {
      stop(paste0("Named fleets (", paste(fleets, collapse = " ") , ") not the same as number of fleets (", nfleets, ")" ))
    }
  }
  if(missing(fleets)) {
    nfleets <- (ncol(rba) - 4) / 3
    fleets <- as.character(1:nfleets)
  }
  txty <- paste(c("sigmaU", "qU", "pU"),c(matrix(fleets,3, length(fleets),byrow=T)),sep="")
  names(rba)[5:ncol(rba)] <- txty


  if(missing(run)) run <- basename(path)

  rba <-
    rba %>%
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


mup_par <- function(path, run) {

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

#' Reads ADMB hst files
#'
#' Reads output of ADMB MCMC report containing estimated distributions. The .hst
#' report contains information about the MCMC analysis: the sample sizes
#' (specied with the -mcmc command-line option), the step size scaling
#' factor, the step sizes, and information about the posterior probability
#' distribution (e.g., the mean, standard deviation, and lower and upper
#' bounds). For each simulated parameter, a range of values (with step sizes
#' reported in the "step sizes" section of the .hst file) and their simulated
#' posterior probabilities is reported. Plotting the first column
#' (parameter values) on the x-axis and the second column (simulated
#' probabilities) on the y-axis can be a convenient way to make a visualization
#' of the posterior probability distribution.
#'
#' @param path Name of the \emph{directory} that contains the result.
#' @param txt The parameters to extract
#' @param startyear Assessment start year
#' @param names A character vector of length two replacing the default names
#' \emph{c('value','prop')}
#' @param negative.allowed Flag, default is FALSE
#'
#' @return A list, each component being a dataframe with two columns. The
#' default names are \emph{c('value','prop')} which can be replace by specifying
#' \emph{names}.
mup_hst <- function (path, txt, startyear = 1955, names, negative.allowed = FALSE) {
  file <- paste(path, "muppet.hst", sep = "/")
  tmpskra <- tempfile("bayes")
  on.exit(unlink(tmpskra))
  tmp <- scan(file, what = character(), sep = "\n", quiet = TRUE)
  tmp1 <- matrix(tmp, length(tmp), 1)
  utils::write.table(tmp1, file = tmpskra, sep = "", col.names = F,
                     row.names = F, quote = F)
  i <- grep(txt, tmp)
  j <- grep("#", tmp)
  j <- j[j > i[length(i)]]

  if (length(j) > 0) {
    j <- j[1] - 1
  } else {
    j <- length(tmp)
  }

  i1 <- i[1:(length(i))] + 1
  i2 <- c(i[2:length(i)] - 1, j)

  if (length(i) == 1)  i2 <- j

  Result <- list()
  for (i in 1:length(i1)) {
    #print(i)
    x <- getlineswin(tmpskra, i1[i], i2[i])
    names(x) <- c('value','prop')
    if (!negative.allowed) x <- x[x$value >= 0, ]
    Result[[i]] <- getlineswin(tmpskra, i1[i], i2[i])
    names(Result[[i]]) <- c('value','prob')
    #Result[[i]] <- calcprofile(Result[[i]], negative.allowed = negative.allowed)
  }
  if (!missing(startyear)) {
    names(Result) <- paste((startyear:(startyear + length(Result) - 1)),
                           sep = "")
    #attributes(Result)$years <- startyear:(startyear + length(Result) -                                          1)
  }
  # if (!missing(names)) names(Result) <- names
  # if (length(Result) == 1) Result <- Result[[1]]
  Result <-
    dplyr::bind_rows(Result, .id = "year") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = as.integer(year))

  return(Result)
}


getlineswin <- function (file, line1, line2) {
  tmpskra <- tempfile("bayes")
  on.exit(unlink(tmpskra))
  #if (missing(nlines)) nlines <- length(count.fields(file, sep = "\n"))
  x <- scan(file, sep = "\t", what = character(), quiet = TRUE)
  x <- matrix(x, length(x), 1)
  x <- x[line1:line2, ]
  utils::write.table(x, file = tmpskra, sep = "\n", col.names = F,
                     row.names = F, quote = F)
  return(utils::read.table(tmpskra))
}


#' @title read muppet output
#'
#' @description reads muppet results
#'
#' @param path A directory path
#' @param scale A scaler (default 1)
#' @param fleets xxx
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

mup_rbx <- function(path, scale = 1, fleets, assyear, run, wide = TRUE) {

  if(!dir.exists(path)) {
    stop(paste0("File path: '", path, "' does not exist"))
  }

  if(missing(run)) run <- basename(path)

  if(missing(scale)) scale <- 1

  list(rby  = mup_rby(path, scale, fleets = fleets, assyear = assyear, run = run),
       rbya = mup_rbya(path, scale, fleets = fleets, assyear = assyear, run, wide),
       rba  = mup_rba(path, fleets = fleets, run = run),
       opr  = mup_opr(path, scale, assyear = assyear, run = run),
       std  = mup_std(path),
       par  = mup_par(path))

}

mup_covar <- function(sigma,rho){
  rho <- rho[1]
  n <- length(sigma)
  x <- diag(sigma^2)
  for(i in 1:n){
    for(j in 1:i){
      x[i,j] <- sigma[i]*sigma[j]*rho^(i-j)
    }
  }
  x <- x + t(x) - diag(sigma^2)
  chol(solve(x))
}

mup_scaled_residuals <- function(rbya,
                          rba,
                          cn=c("rU1","cvU1"),
                          years=1985:2020,
                          ages=1:13,
                          corrcoeff){
  dat <- rba[rba$age %in% ages,] %>% as.data.frame() # Not sure how to do this with filter.
  dat1 <- rbya[rbya$age %in% ages & rbya$year %in% years,] %>% as.data.frame()
  xx <- mup_covar(dat[,cn[2]],corrcoeff)
  x <- tapply(dat1[,cn[1]],list(dat1$year,dat1$age),sum)
  newcn <- paste("sc", cn[1],sep="")
  x1 <-  x %*% t(xx)
  dat1[,newcn] <- c(t(x1))

  return(dat1)

}


# d <- mup_scaled_residuals(dat$rbya,dat$rba,years=1985:2022,ages=2:10,corrcoeff=0.576)
# residplot(d, cn=c("year","age","scrU1"),maxn=2,poscol="red",negcol="blue",maxsize=0.1)




# HOSKI ------------------------------------------------------------------------
# Hér sérðu dæmi um nýja read_separ (read_separ1) í bili
# notkun á því og útkomu.
# res <- read_separ1(".",".",fleets=c("3","1","2","4"),assYear=year+1)
#
# Floti 1 er alltaf mars survey , 2 er haustrall 3 er marssurvey 1-2 og 4 haustrall12 það er út af þessu logq1 dæmi
#
# Við höldum alltaf þessum nöfnum þ.a í haustrallkeyrslu er bara oU2 og pU2 ekki oU1 og pU1.
#
# Ef vilja hafa þetta öðruvísi er defaultið að númera flotanna í vaxandi röð.
# read_separ1 <- function (path, run, rName = NA, mName = NA, calcSurBio = F,
#                          ggFactor = T, Scale = 1000, assYear = NA, retroY = NA,fleets)
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
#   if(missing(fleets)){ # to have some default
#     # Test number of columns
#     if (is.na(retroY))
#       rby <- read.table(paste(path, run, "resultsbyyear", sep = "/"),
#                         header = T, na.strings = c("-1", "0"))
#     if (!is.na(retroY))
#       rby <- read.table(paste(paste(path, run, "resultsbyyear",
#                                     sep = "/"), retroY, sep = ""),
#                         header = T, na.strings = c("-1", "0"))
#     nfleets <- (ncol(rby)-14)/2
#     fleets <- as.character(1:nfleets)
#   }
#   txty <- paste(c("oU","pU"),c(matrix(fleets,2,length(fleets),byrow=T)),sep="")
#   cnRby <- c("year", "r", "n3", "n6", "bioF", "bio", "bio1",
#              "ssb", "ssb2", "fbar", "hr", "oY", "pY",txty,"run", "model")
#   txtya <- paste(c("oU","pU","rU"),c(matrix(fleets,3,length(fleets),byrow=T)),sep="")
#   cnRbya <- c("year", "age", "oC", "cW", "sW", "ssbW", "mat",
#               "n", "z", "f", "m", "pC", "rC",txtya)
#   txta<- paste(c("cvU","qU","pU"),c(matrix(fleets,3,length(fleets),byrow=T)),sep="")
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
#     names(surveybiopow) <- fleets
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
#   res <- read_separ1(".",".",fleets=c("3","1","2","4"),assYear=year+1)
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

