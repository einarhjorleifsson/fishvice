# TODO
#  * Get pM and pF for mup_rbya - ideally this should be returned by muppet in
#    resultsbyyearandage
#  * Question what to do about ssbW vs sW, the latter is used in the sam returns
#    for SSB weights.


#' @title muppet_rbya
#'
#' @description reads muppet year-and-age input and output
#'
#' @param path A directory path
#' @param scale A scaler (default 1)
#' @param long A boolean indicating if returned table long (default TRUE) with variables
#' as names within column 'var' and values in column 'val'. Alternative (FALSE) not yet active.
#' @param run Name of the run, if missing (default) will use the directory name
#' @param assyear Assessment year, if missing (default) will use the year after the last
#' catch at age input
#'
#' @return A tibble

mup_rbya <- function(path, scale = 1, long = TRUE, run, assyear) {

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
                  n = n / scale,
                  run = run,
                  model = "mup",
                  assyear = assyear,
                  yc = year - age)

  if(long) {
    rbya <-
      rbya %>%
      dplyr::select(-c(pC, rC, oU1, oU2, pU1, pU2, rU1, rU2)) %>%
      tidyr::gather(var, val, -c(year, age, run, model, assyear, yc)) %>%
      dplyr::select(year, age, var, val, dplyr::everything())
  }

  return(rbya)

}


mup_rby <- function(fit, scale = 1, run, assyear) {

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

