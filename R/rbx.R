# TODO
#      calculate and return standardized residuals
#      question if one should not return opr-tibble, aka what is done for sam
#      check husky's read_separ1, for missing elements here
#      return all par estimates?


#' read muppet output
#'
#' @param path A vector containing the path to the assessment directory
#' @param Scale A numeric, specifying rescaling (default is 1)
#' @param run  Name of the run, if missing (default) will use the directory name
#' @param assyear Assessment year, if missing (default) will use the last
#'                year of the catch at age matrix plus 1.
#'
#' @return A list containing tibbles
#' @export
#'
read_muppet <- function (path,
                         Scale = 1,
                         run,
                         assyear)
{

  #if(!missing(run)) path <- file.path(path, name)

  if(missing(run)) run <- basename(path)

  # rbya ---
  rbya <-
    read_tsv(file.path(path, "resultsbyyearandage"),
             na = c("-1", "0"))
  if(missing(assyear)) {
    assyear <- rbya %>% filter(!is.na(ObsCno)) %>% pull(year) %>% max()
    assyear <- assyear + 1
  }

  if (ncol(rbya) != 19) {
    rbya$pU2 <- NA
    rbya$oU2 <- NA
    rbya$rU2 <- NA
  }

  rbya <-
    rbya %>%
    select(year, age,
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
    mutate(oC = oC / Scale,
           pC = pC  / Scale,
           cW = cW / Scale,
           sW = sW / Scale,
           n = n / Scale,
           run = run,
           model = "mup",
           assyear = assyear,
           yc = year - age)

  # for testing
  #path <- "/net/hafkaldi.hafro.is/export/u2/reikn/Tac/2021/01/ass/mup/smx"

  # rby ---
  rby <- read_tsv(file.path(path, "resultsbyyear"),
                  na = c("-1", "0"))
  nfleets <- (ncol(rby) - 14) / 2
  fleetnames <- as.character(1:nfleets)
  txty <- paste(c("pU","oU"),c(matrix(fleetnames,2,length(fleetnames),byrow=T)),sep="")
  txtya <- paste(c("pU","oU","rU"),c(matrix(fleetnames,3,length(fleetnames),byrow=T)),sep="")
  txta <- paste(c("cvU","qU","pU"),c(matrix(fleetnames,3,length(fleetnames),byrow=T)),sep="")

  names(rby)[15:ncol(rby)] <- txty

  # dummies
  #if (ncol(rby) != 18) {
  #  rby$pU2 <- NA_real_
  #  rby$oU2 <- NA_real_
  #}

  rby <-
    rby %>%
    rename(r = Recruitment,
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
    mutate(y = ifelse(is.na(oY), pY, oY),
           hr_old = y/bio,
           hr = (1/3 * y + 3/4 * lead(y)) / bio,
           #hr1 = y / bio1,
           #hr2 = y / bio2,
           r = r / Scale) %>%
    select(year:fbar, hr, pY, oY, everything()) %>%
    select(-y) %>%
    #select(year, bio, ssb, r, hr, fbar, oY)
    mutate(run = run,
           model = "mup",
           assyear = assyear)





  # rba ---
  rba <-
    read_tsv(file.path(path, "resultsbyage"),
             na = c("-1", "0"))
  if (ncol(rba) != 10) {
    rba$cvU2 <- NA_real_
    rba$qU2 <- NA_real_
    rba$pU2 <- NA_real_
  }
  rba <-
    rba %>%
    select(age,
           sel = meansel,
           pSel = progsel,
           cvC = SigmaC,
           cvU1 = SigmaSurvey1,
           qU1 = SurveylnQ1,
           pU1 = SurveyPower1,
           cvU2 = SigmaSurvey2,
           qU2 = SurveylnQ2,
           pU2 = SurveyPower2) %>%
    mutate(run = run,
           model = "mup",
           assyear = assyear)



  # ---
  pth <- file.path(path, "muppet.par")
  if(file.exists(pth)) { # get AIC could look for as.numeric=T

    dat <- scan(pth, what = character(), sep = " ", quiet =T )[1:12]
    aicinfo <- as.numeric(c("0",dat[6],dat[11]))
    names(aicinfo) <- c("npar1","npar","objective")
    dat <- scan(pth, what = character(), sep = "\n", quiet = T)
    i <- grep("# surveybiopow",dat)
    surveybiopow <- as.numeric(dat[i+1])
    dat <- scan(pth, what = character(), sep = "\n", quiet = T)
    i <- grep("# Surveycorr",dat)
    surveycorr <- as.numeric(dat[i+1])
  }

  pth <- file.path(path, "muppet.std")
  if(file.exists(pth)) {# Better info about number of par
    dat <- read.table(pth, header = T)
    i <- c(grep("RefF", dat$name), grep("Spawningstock", dat$name))
    dat <- dat[-i,]
    j <- dat$std.dev < 50
    aicinfo["npar1"] <- nrow(dat[j,])
  }

  res <- list(rby = rby, rbya = rbya, rba = rba)

  if(exists("aicinfo")) res$aicinfo <- aicinfo
  if(exists("surveybiopow")){
    #names(surveybiopow) <- fleetnames
    res$surveybiopow <- surveybiopow
  }
  if(exists("surveycorr")){
    #names(surveycorr) <- fleetnames
    res$surveycorr <- surveycorr
  }

  return(res)
}