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
  cmd <- paste0("wget --recursive --reject=png --level=0 --no-parent ",path,"/")
  system(cmd)

  # cleanup
  Path <- "www.stockassessment.org/datadisk/stockassessment/userdirs"
  Path <- paste(Path,user,assessment,sep="/")
  cmd <- paste("mv", Path, ".")
  system(cmd)
  system("rm -r www.stockassessment.org")

}


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

  # Get the results
  fit <- list()

  file <- get_file(paste(URL,"run",sep="/"),"sam.par")
  parfile <- as.numeric(scan(file,
                             what='', n=16, quiet=TRUE)[c(6,11,16)])
  fit$nopar <- as.integer(parfile[1])
  fit$nlogl <- parfile[2]
  fit$maxgrad <- parfile[3]

  file <- get_file(paste(URL,"run",sep="/"),"sam.rep")
  rep <- scan(file, quiet=TRUE)

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

  minAge <- min(fit$res[,3])
  maxAge <- max(fit$res[,3][fit$res[,3]<98.5])
  noN <- maxAge - minAge+1
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
  rbya <- plyr::join(rbya,ibya,by=c("year","age"))

  return(list(rbya=rbya,fit=fit))
}

