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

#' Reads 'adcam' assessment results
#'
#' Some longer text here
#'
#' @export
#' @param path Path to the runs, here the 'root' path to the runs.
#' @param run Name of the \emph{directory} that contains the result.
#' @param rName Name of the run.
#' @param mName Name of the model used.
#' @param calcSurBio Flag, TRUE (default) if survey biomass should be calculated.
#' @param ggFactor If TRUE (default) rescale prerecruits with M=0.0
#' @param Scale Convertion of values
#' @param assYear Assessment year
#' @param retroY The retrospective year
#' @return A list with \code{data.frame} rby, rbya and rba
#' @seealso \code{\link{read_separ}} for reading separate model output and \code{\link{read_adapt}} for reading adapt model output
read_adcam <- function (path,run,rName=NA,mName=NA,calcSurBio=T,ggFactor=T,Scale=1e3,assYear=NA,retroY=NA) {
  cnRby <- c("year","r","n3","n6","bioF","bio","bio1","ssb","ssb2","fbar","hr",
    "oY","pY","oU1","pU1","oU2","pU2","run","model")
  cnRbya <- c("year","age","oC","cW","sW","ssbW","mat","n","z","f","m",
    "pC","rC","oU1","pU1","rU1","oU2","pU2","rU2")
  cnRba <- c("age","sel","pSel","sigma","cvU1","qU1","pU1","cvU2","qU2","pU2","run","model")
  # rby
  if(is.na(retroY)) rby <- read.table(paste(path,run,"resultsbyyear",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rby <- read.table(paste(paste(path,run,"resultsbyyear",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rby)
  if(ncol(rby)!=21) {
    rby$pU2 <- NA
    rby$oU2 <- NA
    rby <- rby[,c(1:17,20:21,18:19)]
  }
  names(rby) <- c("year","F2","fbar","pY","ssb","ssb2","bioF","bio",
    "bio1","r","n3","n6","hr","oY","a50","pU1","oU1",
    "pU2","oU2","n","qF")

  rby$r <- rby$r/Scale
  if(ggFactor) rby$r <- rby$r*exp(-0.4)
  rby$r <- c(rby$r[2:nrow(rby)],NA)
  # overwriting the hr calculated by hoski
  rby$hr <- ifelse(!is.na(rby$oY),rby$oY/rby$bio,rby$pY/rby$bio)
  rby$run <- rName
  rby$model <- mName
  rby <- rby[,cnRby]
  rby$n3 <- rby$n3/Scale
  rby$n6 <- rby$n6/Scale
  # rbyaa
  if(is.na(retroY)) rbya <- read.table(paste(path,run,"resultsbyyearandage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rbya <- read.table(paste(paste(path,run,"resultsbyyearandage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rbya)
  if(ncol(rbya)<18) {
    if(ncol(rbya)==16) rbya$SSB <- rbya$N*rbya$SSBwts*rbya$StockSexmat/1e6
    rbya$pU2 <- NA
    rbya$oU2 <- NA
    rbya$rU2  <- NA
    rbya <- rbya[,c(1:16,18:20,17)]
  }
  names(rbya) <- c("year","age","n","z","sW","m","f","pC","cW","ssbW","mat",
    "matC","oC","pU1","oU1","rU1","pU2","oU2","rU2","ssb")
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 1,rbya$n*exp(-0.4),rbya$n)
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 2,rbya$n*exp(-0.2),rbya$n)
  rbya$rC <- log(rbya$oC/rbya$pC)
  rbya <- rbya[,cnRbya]
  rbya$run <- rName
  rbya$model <- mName
  rbya$oC <- rbya$oC/Scale
  rbya$cW <- rbya$cW/Scale
  rbya$sW <- rbya$sW/Scale
  rbya$n  <- rbya$n/Scale
  rbya$pC <- rbya$pC/Scale
  # rba
  if(is.na(retroY)) rba <- read.table(paste(path,run,"resultsbyage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rba <- read.table(paste(paste(path,run,"resultsbyage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rba)
  if(ncol(rba)!=11) {
    rba$cvU2 <- rep(NA,n)
    rba$qU2  <- rep(NA,n)
    rba$pU2  <- rep(NA,n)
    rba <- rba[,c(1:5,9:11,6:8)]
  }
  names(rba) <- c("age","m","cvU1","qU1","pU1","cvU2","qU2","pU2","sel","pSel","sigma")
  rba$run <- rName
  rba$model <- mName
  rba <- rba[,cnRba]
  if(!is.na(retroY)) {
    #print(retroY)
    rby$assYear <- as.numeric(retroY)+1
    rbya$assYear <- as.numeric(retroY)+1
    rba$assYear <- as.numeric(retroY)+1
  } else {
    rby$assYear <- assYear
    rbya$assYear <- assYear
    rba$assYear <- assYear
  }
  return(list(rby=rby,rbya=rbya,rba=rba))
}


#' Reads 'adapt' assessment results
#'
#' Some longer text here
#'
#' @export
#' @param path Path to the runs, here the 'root' path to the runs.
#' @param run Name of the \emph{directory} that contains the result.
#' @param rName Name of the run.
#' @param mName Name of the model used.
#' @param calcSurBio Flag, TRUE (default) if survey biomass should be calculated.
#' @param ggFactor If TRUE (default) rescale prerecruits with M=0.0
#' @param Scale Convertion of values
#' @param assYear Assessment year
#' @param retroY The retrospective year
#' @return A list with \code{data.frame} rby, rbya and rba
#' @seealso \code{\link{read_separ}} for reading separate model output and \code{\link{read_adcam}} for reading adcam model output
read_adapt <- function (path,run,rName=NA,mName=NA,calcSurBio=F,ggFactor=T,Scale=1e3,assYear=NA,retroY=NA) {

  cnRby <- c("year","r","n3","n6","bioF","bio","bio1","ssb","ssb2","fbar","hr",
    "oY","pY","oU1","pU1","oU2","pU2","run","model")
  cnRbya <- c("year","age","oC","cW","sW","ssbW","mat","n","z","f","m",
    "pC","rC","oU1","pU1","rU1","oU2","pU2","rU2")
  cnRba <- c("age","sel","pSel","sigma","cvU1","qU1","pU1","cvU2","qU2","pU2","run","model")

  # rby
  if(is.na(retroY)) rby <- read.table(paste(path,run,"resultsbyyear",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rby <- read.table(paste(paste(path,run,"resultsbyyear",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rby)
  if(ncol(rby)!=18) {
    rby$pU2 <- rep(NA,n)
    rby$oU2 <- rep(NA,n)
  }
  names(rby) <- c("year","fbar","pY","oY","ssb","ssb2","bioF","bio1",
    "bio","preR","r","n1","n3","n6","pU1","oU1",
    "pU2","oU2")
  if(ggFactor) rby$r <- rby$r*exp(-0.4)
  rby$hr <- ifelse(!is.na(rby$oY),rby$oY,rby$pY)/rby$bio
  rby$run <- rName
  rby$model <- mName
  rby <- rby[,cnRby]

  rby$r <- rby$r/Scale
  rby$n3 <- rby$n3/Scale
  rby$n6 <- rby$n6/Scale


  # rbya
  if(is.na(retroY)) rbya <- read.table(paste(path,run,"resultsbyyearandage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rbya <- read.table(paste(paste(path,run,"resultsbyyearandage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rby)
  if(ncol(rbya) != 19) {
    rbya$pU2 <- NA
    rbya$oU2 <- NA
    rbya$rU2  <- NA
  }
  names(rbya) <- c("year","age","n","z","sW","m","f","pC","cW","ssbW","mat",
    "oC","rC","pU1","oU1","rU1","pU2","oU2","rU2")
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 1,rbya$n*exp(-0.4),rbya$n)
  if(ggFactor) rbya$n <- ifelse(rbya$age %in% 2,rbya$n*exp(-0.2),rbya$n)

  rbya <- rbya[,cnRbya]
  rbya$run <- rName
  rbya$model <- mName
  rbya$oC <- rbya$oC/Scale
  rbya$cW <- rbya$cW/Scale
  rbya$sW <- rbya$sW/Scale
  rbya$n  <- rbya$n/Scale
  rbya$pC <- rbya$pC/Scale

  # rba
  if(is.na(retroY)) rba <- read.table(paste(path,run,"resultsbyage",sep="/"),header=T,na.strings=c("-1","0"))
  if(!is.na(retroY)) {
    rba <- read.table(paste(paste(path,run,"resultsbyage",sep="/"),retroY,sep=""),header=T,na.strings=c("-1","0"))
  }
  n <- nrow(rba)
  if(ncol(rba)!=10) {
    rba$cvU2 <- rep(NA,n)
    rba$qU2  <- rep(NA,n)
    rba$pU2  <- rep(NA,n)
  }
  names(rba) <- c("age","sel","pSel","sigma","cvU1","qU1","pU1","cvU2","qU2","pU2")
  rba$run <- rName
  rba$model <- mName
  rba <- rba[,cnRba]
  if(!is.na(retroY)) {
    print(retroY)
    rby$assYear <- as.numeric(retroY)+1
    rbya$assYear <- as.numeric(retroY)+1
    rba$assYear <- as.numeric(retroY)+1
  } else {
    rby$assYear <- assYear
    rbya$assYear <- assYear
    rba$assYear <- assYear
  }

  return(list(rby=rby,rbya=rbya,rba=rba))
}

#' Reads 'separable' assessment results
#'
#' Some longer text here
#'
#' @export
#' @param path Path to the runs, here the 'root' path to the runs.
#' @param run Name of the \emph{directory} that contains the result.
#' @param rName Name of the run.
#' @param mName Name of the model used.
#' @param calcSurBio Flag, TRUE (default) if survey biomass should be calculated.
#' @param ggFactor If TRUE (default) rescale prerecruits with M=0.0
#' @param Scale Convertion of values
#' @param assYear Assessment year
#' @param retroY The retrospective year
#' @return A list with \code{data.frame} rby, rbya and rba
#' @note If file exist it is simply overwritten without any warning
#' @seealso \code{\link{read_adcam}} for reading adcam model output and \code{\link{read_adapt}} for reading adapt model output
read_separ <- read_adapt  # just to avoid (create?) confusion
