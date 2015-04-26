#' @title sam process error
#'
#' @description XXX
#'
#' @param rbya XXX
#' @param plus_group XXX
#'
sam_process_error <- function(rbya, plus_group=TRUE) {

  # ----------------------------------------------------------------------------
  # align the year-classes
  x <- rbya[,c("year","age","n")]
  x$year <- x$year - 1
  x$age <- x$age - 1
  names(x)[3] <- "n.end"
  d <- plyr::join(rbya[,c("year","age","n","m","f","cW")],x, by=c("year","age"))
  d <- d[!is.na(d$n.end),]

  # ----------------------------------------------------------------------------
  # exclude plus-group
  if(plus_group) d <- d[d$age < max(d$age),]

  # ----------------------------------------------------------------------------
  # process error expressed as mortality
  d$z.n <- log(d$n/d$n.end)
  d$z.f <- d$f + d$m
  d$z.d  <- d$z.n - d$z.f

  # ----------------------------------------------------------------------------
  # process error expressed as numbers
  d$n.end2 <- d$n * exp(-(d$f + d$m))
  # Calculate the difference
  d$n.d <- d$n.end - d$n.end2

  # ----------------------------------------------------------------------------
  # process errror expressed as biomass
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE)/1e3)
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE)/1e3)

  return(list(rbya=d, rby=x))

}





