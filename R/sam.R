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

sam_get_directory <- function(assessment, user="user3") {

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
# read_sam is the next generation of the function above

#' @title read_sam
#'
#' @description Gets the input files and sam output files from the www.stockassessment.org
#'
#' @export
#'
#' @param directory The directory name that contains the sam run.
#' @param from_web If FALSE (default) read from local directory. If TRUE read
#' from www.stockassessment.org
#' @param user User name if reading from www.stockassessment.org, guest users can use "user3" (default)

read_sam <- function(directory="WBcod_2015_short", from_web=FALSE, user="user3") {

  # dummies
  cW <- oC <- 0

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

  keys <- list()


  # ----------------------------------------------------------------------------
  # The input data
  keys$stateDim <- scan(sam.rep, quiet=TRUE)[1]

  lin <- readLines(sam.dat)
  lin <- lin[!lin==""]
  lin <- stringr::str_trim(lin)
  # lines without a starting #
  i <- !stringr::str_locate(lin,"#")[,1] %in% 1
  lin <- lin[i]

  keys$nFleets <- as.integer(lin[1])
  keys$nYears  <- as.integer(lin[4])
  keys$years <-   as.integer(strsplit(lin[5]," ")[[1]])

  idx1 <- as.integer(strsplit(lin[7]," ")[[1]])
  idx2 <- as.integer(strsplit(lin[8]," ")[[1]])

  lin <- lin[9:length(lin)]
  sublin <- lin[1:max(idx2)]
  ibya <- as.data.frame(matrix(as.numeric(unlist(strsplit(sublin," "))),ncol = 4, byrow = TRUE))
  colnames(ibya) <- c("year","fleet","age","obs")
  ibya <- reshape2::dcast(ibya, year + age ~ fleet, value.var = "obs")
  colnames(ibya) <- c("year","age","oC",paste0("oU",1:(ncol(ibya)-3)))

  keys$minAge <- min(ibya$age)
  keys$maxAge <- max(ibya$age)
  keys$nAges <- length(unique(ibya$age))
  keys$ages <- c(keys$minAge:keys$maxAge)

  sublin <- lin[(max(idx2)+1):length(lin)]
  x <- as.data.frame(matrix(as.numeric(unlist(strsplit(sublin," "))),ncol = keys$nAges, byrow = TRUE))
  names(x) <- c(keys$minAge:keys$maxAge)
  x$year <- c(keys$years,                        # proportion mature
              keys$years,                        # stock weights
              keys$years[-length(keys$years)],   # catch weights
              keys$years,                        # natural mortality
              keys$years[-length(keys$years)],   # landing fraction
              keys$years[-length(keys$years)],   # discard weights
              keys$years[-length(keys$years)],   # landing weights
              keys$years,                        # pF
              keys$years)                        # pM
  n1 <- length(keys$years)
  n2 <- n1 - 1
  x$variables <- c(rep("mat",n1),
                   rep("sW",n1),
                   rep("cW",n2),
                   rep("m", n1),
                   rep("fL",n2),
                   rep("dW",n2),
                   rep("lW",n2),
                   rep("pF",n1),
                   rep("pM",n1))
  x <- reshape2::melt(x,c("year","variables"),variable.name="age")
  x <- reshape2::dcast(x, year + age ~ variables, value.var = "value")
  x$age <- as.integer(as.character(x$age))
  ibya <- plyr::join(x,ibya,by=c("year","age"))

  # ----------------------------------------------------------------------------
  # Dimensions
  lin <- readLines(model.cfg, warn=FALSE)
  lin <- lin[!lin==""]
  lin <- stringr::str_trim(lin)
  # lines without a starting #
  i <- !stringr::str_locate(lin,"#")[,1] %in% 1
  lin <- lin[i]

  keys$plusGroup <- lin[3] == 0
  keys$states <- as.integer(stringr::str_split(lin[4],"\t")[[1]][1:keys$nAges])



  # ----------------------------------------------------------------------------
  # residuals
  res <- read.table(sam.res,header=FALSE)
  colnames(res) <- c("year","fleet","age","obs","pre","res")
  res$pre <- exp(res$pre)

  x <- reshape2::dcast(res, year + age ~ fleet, value.var="pre")
  colnames(x) <- c("year","age","pC",paste0("pU",1:(ncol(x)-3)))
  rbya <- plyr::join(ibya,x,by=c("year","age"))
  x <- reshape2::dcast(res, year + age ~ fleet, value.var="res")
  colnames(x) <- c("year","age","rC",paste0("rU",1:(ncol(x)-3)))
  rbya <- plyr::join(rbya,x,by=c("year","age"))

  # ----------------------------------------------------------------------------
  #
  lin <- readLines(sam.cor)
  fit <- list()
  fit$npar <- length(lin)-2
  fit$logDetHess <- as.numeric(strsplit(lin[1], '=')[[1]][2])
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

  rbya <- plyr::join(x,rbya,by=c("year","age"))

  # ----------------------------------------------------------------------------
  # Calculate oY
  oY <- plyr::ddply(rbya, c("year"), plyr::summarise,
                    oY = sum(cW * oC, na.rm=TRUE))
  rby <- plyr::join(rby, oY, by=c("year"))

  return(list(rbya=rbya,rby=rby))

}


# ------------------------------------------------------------------------------
# redundant function

#' @title sam_get_results
#'
#' @description Gets the input files and sam output files from the www.stockassessment.org
#'
#' @export
#'
#' @param assessment The directory name of the assessment as specified on
#' www.stockassessment.org
#' @param user User name, guest users can use "user3" (default)

sam_get_results <- function(assessment="WBcod_2015_short",user="user3") {

  URL <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",user,assessment,sep="/")

  # Get the results
  fit <- list()

  file <- get_file(paste(URL,"run",sep="/"),"sam.par")

  parfile <- as.numeric(scan(file,
                             what='', n=16, quiet=TRUE)[c(6,11,16)])
  fit$nopar <- as.integer(parfile[1])
  fit$nlogl <- parfile[2]
  fit$maxgrad <- parfile[3]


  file <- get_file(paste(URL,"run",sep="/"),"sam.rep")

  rep  <- scan(file, quiet=TRUE)

  file <- get_file(paste(URL,"run",sep="/"),"sam.res")
  fit$res <- read.table(file,header=FALSE)

  fit$stateDim <- rep[1]
  fit$years <- rep[-1]

  file <- get_file(paste(URL,"run",sep="/"),"sam.cor")
  lin <- readLines(file)
  fit$npar <- length(lin)-2
  fit$logDetHess <- as.numeric(strsplit(lin[1], '=')[[1]][2])
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

  mslh <- function(name){
    idx <- which(fit$names==name)
    x <- cbind(fit$est[idx], fit$std[idx], fit$est[idx]-2*fit$std[idx],
               fit$est[idx]+2*fit$std[idx])
    colnames(x) <- c('est', 'std', 'low', 'hig')
    return(x)
  }

  #idx <- which(fit$names==name)

  fit$ssb <- mslh('ssb')
  fit$fbar<-mslh('fbar')
  fit$tsb<-mslh('tsb')
  fit$logssb<-mslh('logssb')
  fit$logfbar<-mslh('logfbar')
  fit$logtsb<-mslh('logtsb')
  fit$logscale<-mslh('logScale')
  fit$logFpar<-mslh('logFpar')
  fit$logCatch<-mslh('logCatch')

  x <- mslh('U')
  fit$stateEst <- matrix(x[,1],ncol=fit$stateDim, byrow=TRUE)
  fit$stateStd <- matrix(x[,2],ncol=fit$stateDim, byrow=TRUE)
  fit$stateLow <- matrix(x[,3],ncol=fit$stateDim, byrow=TRUE)
  fit$stateHig <- matrix(x[,4],ncol=fit$stateDim, byrow=TRUE)
  fit$R<-cbind(exp(fit$stateEst[,1]), NA, exp(fit$stateLow[,1]),
               exp(fit$stateHig[,1]))
  #if(reduced){
  #  fit <- fit[which(!names(fit)%in%c('cov','cor'))]
  #}

  file <- get_file(paste(URL,"run",sep="/"),"sam.rep")
  nFleets <- scan(file,comment.char="#",quiet=TRUE)[1]


  #########################################################
  # read the conf file
  file <- get_file(paste(URL,"run",sep="/"),"model.cfg")
  #file <- "WBcod_2015_short/run/model.cfg"
  lin <- readLines(file,warn=FALSE)
  lin <- lin[!lin==""]
  lin <- stringr::str_trim(lin)
  # lines without a starting #
  i <- !stringr::str_locate(lin,"#")[,1] %in% 1
  lin <- lin[i]

  res <- list()
  res$minAge <- as.integer(lin[1])
  res$maxAge <- as.integer(lin[2])
  res$plusGroup <- lin[3] == 0
  nAges <- res$maxAge - res$minAge + 1
  res$states <- as.integer(stringr::str_split(lin[4],"\t")[[1]][1:nAges])
  #names(res$states) <- c(res$minAge:res$maxAge)
  fit$keys <- res
  #####################################################

  # Confclone.log
  file <- get_file(paste(URL,"run",sep="/"),"confclone.log")

  #file <- "WBcod_2015_short/run/confclone.log"

  minAge <- min(fit$res[,3])
  maxAge <- max(fit$res[,3][fit$res[,3]<98.5])
  noN <- maxAge - minAge + 1
  noFleet <- max(fit$res[,2])

  N <- exp(fit$stateEst[,1:noN]) #/Scale
  colnames(N) <- c(minAge:maxAge)
  rownames(N) <- fit$years
  N <- reshape2::melt(N,factorsAsStrings = FALSE)
  names(N) <- c("year","age","n")

  x1 <- exp(fit$stateEst[,-c(1:noN)])
  x2 <- exp(fit$stateEst[,-c(1:noN)])[,fit$keys$states]

  mort <- exp(fit$stateEst[,-c(1:noN)])[,fit$keys$states]
  rownames(mort) <- fit$years

  mort_age <- fit$keys$states
  names(mort_age) <- c(minAge:maxAge)
  mort_age <- mort_age[mort_age>0]
  colnames(mort) <- names(mort_age)

  mort <- reshape2::melt(mort,factorsAsStrings = FALSE)
  names(mort) <- c("year","age","f")

  rbya <- plyr::join(N,mort,by=c("year","age"))
  rbya$f[is.na(rbya$f)] <- 0

  # Get the input files
  # TODO: A loop?
  oc <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"cn.dat"),value.name="oC",format = "long")
  cw <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"cw.dat"),value.name="cW",format = "long")
  sw <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"sw.dat"),value.name="sW",format = "long")
  mat <- read_lowestoft(get_file(paste(URL,"data",sep="/"),"mo.dat"),value.name="mat",format = "long")
  nat <- read_lowestoft(get_file(paste(URL,"data",sep="/"),"nm.dat"),value.name="m",format = "long")
  pf <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"pf.dat"),value.name="pF",format = "long")
  pm <-  read_lowestoft(get_file(paste(URL,"data",sep="/"),"pm.dat"),value.name="pM",format = "long")

  ibya <- plyr::join(oc,sw,by=c("year","age"))
  ibya <- plyr::join(ibya,cw,by=c("year","age"))
  ibya <- plyr::join(ibya,sw,by=c("year","age"))
  ibya <- plyr::join(ibya,mat,by=c("year","age"))
  ibya <- plyr::join(ibya,nat,by=c("year","age"))
  ibya <- plyr::join(ibya,pf,by=c("year","age"))
  ibya <- plyr::join(ibya,pm,by=c("year","age"))


  rbya <- plyr::join(rbya,ibya,by=c("year","age"))

  return(list(rbya=rbya,fit=fit))
}



