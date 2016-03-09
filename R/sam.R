# ------------------------------------------------------------------------------
# various sam functions

#' @title Get a sam directory from stockassessment.org
#'
#' @description The function copies the whole directory of an assessment run from
#' stockassessment.org to a local directory
#'
#' @export
#'
#' @param assessment Name of the assessment
#' @param user Name of the user
#'

get_sam_directory <- function(assessment, user="user3") {

  message("Function will be dropped from this package. See package 'ramsam'")

  path <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",user,assessment,sep="/")
  cmd <- paste0("wget --recursive --reject=png,html --level=0 --no-parent ",path,"/")
  system(cmd)

  # cleanup
  Path <- "www.stockassessment.org/datadisk/stockassessment/userdirs"
  Path <- paste(Path,user,assessment,sep="/")
  cmd <- paste("mv", Path, ".")
  system(cmd)
  system("rm -r www.stockassessment.org")

}


# ------------------------------------------------------------------------------
# read_sam is the next generation of the function below

#' @title read_sam
#'
#' @description Gets the input files and sam output files from the www.stockassessment.org
#'
#' Some important pieces of the code are from Anders Nielsen
#'
#' @export
#'
#' @param directory The directory name that contains the sam run.
#' @param from_web If FALSE (default) read from local directory. If TRUE read
#' from www.stockassessment.org
#' @param user User name if reading from www.stockassessment.org, guest users can use "user3" (default)

read_sam <- function(directory="WBcod_2015_short", from_web=FALSE, user="user3") {

  message("Function will be dropped from this package. See package 'ramsam'")

  # dummies
  cW <- oC <- fleet <- obs <- age <-
    yield_low <- yield_hig <- 0

  if(from_web) {
    URL <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",user,directory,sep="/")
    sam.dat       <- get_file(paste(URL,"run",sep="/"),"sam.dat")
    sam.par       <- get_file(paste(URL,"run",sep="/"),"sam.par")
    sam.rep       <- get_file(paste(URL,"run",sep="/"),"sam.rep")
    sam.res       <- get_file(paste(URL,"run",sep="/"),"sam.res")
    sam.cor       <- get_file(paste(URL,"run",sep="/"),"sam.cor")
    model.cfg     <- get_file(paste(URL,"run",sep="/"),"model.cfg")
    confclone.log <- get_file(paste(URL,"run",sep="/"),"confclone.log")
  } else {
    sam.dat       <- paste0(directory,"/run/sam.dat")
    sam.par       <- paste0(directory,"/run/sam.par")
    sam.rep       <- paste0(directory,"/run/sam.rep")
    sam.res       <- paste0(directory,"/run/sam.res")
    sam.cor       <- paste0(directory,"/run/sam.cor")
    model.cfg     <- paste0(directory,"/run/model.cfg")
    confclone.log <- paste0(directory,"/run/confclone.log")
  }

  # keys will collate various information
  keys <- list()
  keys$stateDim <- scan(sam.rep, quiet=TRUE)[1]

  # ----------------------------------------------------------------------------
  # sam.dat - The observations and auxillary material

  # 0. some tidying
  dat <- readLines(sam.dat)
  dat <- dat[!dat==""]
  dat <- stringr::str_trim(dat)
  # lines without a starting #
  i <- !stringr::str_locate(dat,"#")[,1] %in% 1
  dat <- dat[i]

  # 1. header information
  keys$nFleets <- as.integer(dat[1])
  keys$nYears  <- as.integer(dat[4])
  keys$years <-   as.integer(strsplit(dat[5]," ")[[1]])
  keys$nObs <- as.integer(dat[6])

  idx1 <- as.integer(strsplit(dat[7]," ")[[1]])
  idx2 <- as.integer(strsplit(dat[8]," ")[[1]])

  # chop off header information
  dat <- dat[9:length(dat)]

  # 2. Chunk containing the observation matrix (catch at age and tuning indices)
  #    the data here is stored as a long format (year, fleet, age, obs)
  #    Here we convert the data such that each tuning fleet is a column
  # Note: In the case of e.g. mackerel - in the input files there is age 99
  #       which stands for non-age index (egg survey)

  #ibya <- dat[1:max(idx2)] %>%
  #  strsplit(split = " ") %>%
  #  unlist() %>%
  #  as.numeric() %>%
  #  matrix(ncol = 4, byrow = TRUE) %>%
  #  as.data.frame() %>%
  #  dplyr::rename(year = V1, fleet = V2, age = V3, obs = V4) %>%
  #  reshape2::dcast(year + age ~ fleet, value.var = "obs")
  #colnames(ibya) <- c("year","age","oC",paste0("oU",1:(ncol(ibya)-3)))

  # uses hadley approach
  ibya <- dplyr::data_frame(x = dat[1:max(idx2)]) %>%
    tidyr::separate(col = x,
                    into = c("year","fleet","age","obs"),
                    extra = "merge",
                    convert = TRUE) %>%
    tidyr::spread(key = fleet, value = obs)
  colnames(ibya) <- c("year","age","oC",paste0("oU",1:(ncol(ibya)-3)))

  # 3. Chunk containing the auxillary information - weights and assumptions
  #    i.e. weights, M, pF and pM

  # First get the ages
  ages <- sort(unique(ibya$age))
  ages <- ages[ages != 99]

  keys$minAge <- min(ages)
  keys$maxAge <- max(ages)
  keys$nAges  <- length(ages)
  keys$ages   <- c(keys$minAge:keys$maxAge)

  # 3a. Get weights and assumptions (M, pF and pM)
  # Problem - There may be additional auxillary information (e.g. Mackerel: tagging
  #               data, nsCod: year multipliers on catches)
  #               Hence start by getting the line counts
  n_years_total <- length(keys$years)
  n_years_catch <- length(unique(ibya$year[!is.na(ibya$oC)]))
  # 5 matrices: mat, sW, M, pF, pM
  # 4 matrices: cW, dW, lW, fD
  n <- n_years_total * 5 + n_years_catch * 4
  ibya2 <- dplyr::data_frame(x=dat[(max(idx2)+1):(max(idx2) + n)]) %>%
    tidyr::separate(col=x,
                    into = as.character(ages),
                    sep = "[^[:alnum:]|^[:punct:]]+",
                    convert = TRUE)
  ibya2$variables <- c(rep("mat",n_years_total),
                       rep("sW",n_years_total),
                       rep("cW",n_years_catch),
                       rep("m", n_years_total),
                       rep("fL",n_years_catch),
                       rep("dW",n_years_catch),
                       rep("lW",n_years_catch),
                       rep("pF",n_years_total),
                       rep("pM",n_years_total))
  ibya2$year <- c(keys$years,                        # proportion mature
                  keys$years,                        # stock weights
                  keys$years[1:n_years_catch],       # catch weights
                  keys$years,                        # natural mortality
                  keys$years[1:n_years_catch],       # landing fraction
                  keys$years[1:n_years_catch],       # discard weights
                  keys$years[1:n_years_catch],       # landing weights
                  keys$years,                        # pF
                  keys$years)                        # pM
  ibya <- ibya2 %>%
    reshape2::melt(c("year","variables"),variable.name="age") %>%
    reshape2::dcast(year + age ~ variables, value.var = "value") %>%
    dplyr::mutate(age = as.integer(as.character(age))) %>%
    dplyr::full_join(ibya,by=c("year","age"))

  # 3b. Additional information
  # ... to be implemented, now ignored

  # ----------------------------------------------------------------------------
  # Dimensions
  x <- readLines(model.cfg, warn=FALSE)
  x <- x[!x==""]
  x <- stringr::str_trim(x)
  # lines without a starting #
  i <- !stringr::str_locate(x,"#")[,1] %in% 1
  x <- x[i]

  keys$plusGroup <- x[3] == 0
  keys$states <- as.integer(stringr::str_split(x[4],"\t")[[1]][1:keys$nAges])



  # ----------------------------------------------------------------------------
  # residuals
  res <- read.table(sam.res, header=FALSE)
  colnames(res) <- c("year","fleet","age","obs","pre","res")
  res$pre <- exp(res$pre)

  x <- reshape2::dcast(res, year + age ~ fleet, value.var="pre")
  colnames(x) <- c("year","age","pC",paste0("pU",1:(ncol(x)-3)))
  rbya <- dplyr::full_join(ibya,x,by=c("year","age"))
  x <- reshape2::dcast(res, year + age ~ fleet, value.var="res")
  colnames(x) <- c("year","age","rC",paste0("rU",1:(ncol(x)-3)))
  rbya <- dplyr::full_join(rbya,x,by=c("year","age"))

  # ----------------------------------------------------------------------------
  # This code is from Anders Nielsen
  lin <- readLines(sam.cor)
  fit <- list()
  fit$npar <- length(lin)-2
  i <- stringr::str_locate(lin[1],"=")[2]
  fit$logDetHess <- stringr::str_sub(lin[1],i+1) %>%
    stringr::str_trim() %>%
    as.numeric()
  sublin <- lapply(strsplit(lin[1:fit$npar+2], ' '),function(x)x[x!=''])

  fit$names <- unlist(lapply(sublin,function(x)x[2]))
  fit$est <- as.numeric(unlist(lapply(sublin,function(x)x[3])))
  fit$std <- as.numeric(unlist(lapply(sublin,function(x)x[4])))

  fit$cor<-matrix(NA, fit$npar, fit$npar)
  for(i in 1:fit$npar){
    fit$cor[1:i,i]<-as.numeric(unlist(lapply(sublin[i],
                                             function(x)x[5:(4+i)])))
    fit$cor[i,1:i]<-fit$cor[1:i,i]
  }
  fit$cov<-fit$cor*(fit$std%o%fit$std)

  mslh <- function(name, years) {

    idx <- which(fit$names==name)
    x <- cbind(fit$est[idx], fit$std[idx], fit$est[idx]-2*fit$std[idx],
               fit$est[idx]+2*fit$std[idx])

    if(!missing(years)) {
      x <- as.data.frame(x)
      if(name == "tsb") name <- "bio"
      colnames(x) <- paste0(name,c("","_std","_low","_hig"))
      x$year <- years
    }

    return(x)
  }

  ssb      <- mslh('ssb' ,keys$years)
  fbar     <- mslh('fbar' ,keys$years)
  bio      <- mslh('tsb', keys$years)
  yield    <- mslh('logCatch', keys$years[1:(length(keys$years)-1)])
  names(yield) <- c("yield","yield_std","yield_low","yield_hig","year")
  yield <- yield %>%
    dplyr::mutate(yield = exp(yield),
           yield_low = exp(yield_low),
           yield_hig = exp(yield_hig))


  x <- mslh('U')
  fit$stateEst <- matrix(x[,1],ncol=keys$stateDim, byrow=TRUE)
  fit$stateStd <- matrix(x[,2],ncol=keys$stateDim, byrow=TRUE)
  fit$stateLow <- matrix(x[,3],ncol=keys$stateDim, byrow=TRUE)
  fit$stateHig <- matrix(x[,4],ncol=keys$stateDim, byrow=TRUE)

  rec <- cbind(exp(fit$stateEst[,1]), NA, exp(fit$stateLow[,1]),
               exp(fit$stateHig[,1]))
  rec <- as.data.frame(rec)
  colnames(rec) <- paste0("r",c("","_std","_low","_hig"))
  rec$year <- keys$years

  rby <- plyr::join(ssb,fbar, by="year")
  rby <- plyr::join(rby,bio, by="year")
  rby <- plyr::join(rby,rec, by="year")
  rby <- plyr::join(rby,yield, by = "year")

  #if(reduced){
  #  fit <- fit[which(!names(fit)%in%c('cov','cor'))]
  #}

  N <- exp(fit$stateEst[,1:keys$nAges]) #/Scale
  colnames(N) <- c(keys$minAge:keys$maxAge)
  rownames(N) <- keys$years
  N <- reshape2::melt(N,factorsAsStrings = FALSE)
  names(N) <- c("year","age","n")

  #x <- exp(fit$stateEst[-c(1:keys$nAges),])

  mort <- exp(fit$stateEst[,-c(1:keys$nAges)])[,keys$states]
  rownames(mort) <- keys$years
  #colnames(mort) <- keys$ages

  mort_age <- keys$states
  names(mort_age) <- c(keys$minAge:keys$maxAge)
  mort_age <- mort_age[mort_age > 0]
  colnames(mort) <- names(mort_age)
  mort <- reshape2::melt(mort,factorsAsStrings = FALSE)
  names(mort) <- c("year","age","f")

  x <- plyr::join(N,mort,by=c("year","age"))
  x$f[is.na(x$f)] <- 0

  rbya <- dplyr::full_join(rbya,x,by=c("year","age"))

  # ----------------------------------------------------------------------------
  # Calculate oY
  oY <- plyr::ddply(rbya, c("year"), plyr::summarise,
                    oY = sum(cW * oC, na.rm=TRUE))
  rby <- dplyr::full_join(rby, oY, by=c("year"))

  return(list(rbya=rbya,rby=rby))

}





