#' @title Get a sam directory from stockassessment.org
#'
#' @description The function copies the whole directory of an assessment run from
#' stockassessment.org to a local directory
#'
#' @param assessment Name of the assessment
#' @param user Name of the user (default "user3")
#'

sam_get_directory <- function(assessment, user = "user3") {

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


#' sam_get_fit
#'
#' @param assessment Name of run on stockassessment.org
#'
#' @return A "sam"-object
#'
#' @export
sam_get_fit <- function(assessment) {

  stockassessment::fitfromweb(assessment, character.only = TRUE)

}

#' sam_get_data
#'
#' @param assessment Name of run on stockassessment.org
#'
#' @return A list
#'
sam_get_data <- function(assessment) {

  dat <- NA
  load(url(sub("SN", assessment, "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/data.RData")))
  return(dat)

}

#' sam_get_residuals
#'
#' @param assessment Name of run on stockassessment.org
#'
#' @return XXXX
sam_get_residuals <- function(assessment) {

  dat <- NA
  load(url(sub("SN", assessment, "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/residuals.RData")))
  return(dat)

}

#' sam_get_retro
#'
#' @param assessment Name of run on stockassessment.org
#'
#' @return XXXX
sam_get_retro <- function(assessment) {

  RETRO <- NA
  load(url(sub("SN", assessment, "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/retro.RData")))
  return(RETRO)

}

#' sam_ibya_fromsam
#'
#' @param fit An object of class sam (often named fit by users)
#'
#' @return A tibble

sam_ibya_fromsam <- function(fit) {

  if(class(fit)[[1]] != "sam")
    stop('Object has to be of class "sam"')

  lh <- function(x, cn) {
    x %>%
      as.data.frame() %>%
      dplyr::mutate(year = row.names(.) %>% as.integer()) %>%
      tidyr::gather(age, {{cn}}, -year, convert = TRUE) %>%
      tibble::as_tibble()
  }

  data <- fit$data

  obs <-
    data$aux %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(obs = exp(data$logobs))

  d <-
    obs %>%
    dplyr::filter(fleet == 1) %>%
    dplyr::select(year, age, oC = obs) %>%
    dplyr::full_join(lh(data$catchMeanWeight, cW), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$disMeanWeight, dW), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$landFrac, lF), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$landMeanWeight, lW), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$propMat, mat), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$natMor, m), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$propF, pF), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$propM, pM), by = c("year", "age")) %>%
    dplyr::full_join(lh(data$stockMeanWeight, sW), by = c("year", "age"))

  return(d)

}


#' @title sam_ibya
#'
#' @description sam input data as a tibble
#'
#' @param fit A "sam"-object
#'
#' @return A tibble containing the following variables:
#' \itemize{
#'   \item year:
#'   \item age:
#'   \item cW: Catch weights
#'   \item dW: Discards weights
#'   \item lF: Fraction of fish landed
#'   \item lW: Landed weights
#'   \item m: Assumed natural mortality (M)
#'   \item mat: Maturity ogive
#'   \item oC: Observed catch
#'   \item oU1: Observed survey index 1
#'   \item pU2: Observed survey index 2, etc.
#'   \item pF: Proportional F prior to spawning
#'   \item pM: Proportional M prior to spawning
#'   \item sW: Spawning stock weight
#' }
#'
#' @export
#'
#'

sam_ibya <- function(fit) {

  if(class(fit)[[1]] == "sam") {
    return(sam_ibya_fromsam(fit))
  }

  # # Older code, reading stuff from text-files
  # fit <- stockassessment::fitfromweb(assessment, character.only = TRUE)
  #
  # if(fromweb) {
  #   assessment <-
  #     file.path("https://www.stockassessment.org/datadisk/stockassessment/userdirs",
  #               user,
  #               assessment,
  #               "data")
  # } else {
  #   assessment <- file.path(assessment, "data")
  # }
  #
  # files <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat",
  #            "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat")
  # var <- c("oC", "cW", "dW", "lF", "lW",
  #          "mat", "m", "pF", "pM", "sW")
  # res <- list()
  #
  # for(i in 1:length(files)) {
  #
  #   res[[i]] <-
  #     stockassessment::read.ices(paste0(assessment, "/", files[i])) %>%
  #     as.data.frame() %>%
  #     dplyr::mutate(year = row.names(.) %>% as.integer()) %>%
  #     tidyr::gather(age, value, -year, convert = TRUE) %>%
  #     dplyr::mutate(variable = var[i]) %>%
  #     tibble::as_tibble()
  # }
  #
  #
  # x <- stockassessment::read.ices(paste0(path, "/", "survey.dat"))
  # sur <- list()
  # for(i in 1:length(x)) {
  #   sur[[i]] <-
  #     x[[i]] %>%
  #     as.data.frame() %>%
  #     dplyr::mutate(year = row.names(.) %>% as.integer()) %>%
  #     tidyr::gather(age, value, -year, convert = TRUE) %>%
  #     dplyr::mutate(variable = paste0("oU", i)) %>%
  #     tibble::as_tibble()
  # }
  #
  # sur <-
  #   sur %>%
  #   dplyr::bind_rows()
  #
  # res %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::bind_rows(sur) %>%
  #   tidyr::spread(variable, value) %>%
  #   tibble::as_tibble() %>%
  #   return()

}

# TO DO: add the residuals

#' @title sam_rbya
#'
#' @description Extracts stock in numbers (Nay) and fishing mortality (Fay) at
#' age from object "sam". If a tibble ibya (e.g. generated by {sam_ibya} is
#' included in the argument, these estimates get added to that tibble.
#'
#' @param fit A "sam" object
#' @param ibya A tibble containing input data by year and age
#'
#' @return A tibble, containing at minimum:
#' \itemize{
#'   \item year:
#'   \item age:
#'   \item n: stock in numbers
#'   \item f: fishing mortality
#' }
#'
#' @export
#'
sam_rbya <- function(fit, ibya) {

  nay <-
    stockassessment::ntable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer()) %>%
    tidyr::gather(age, n, -year, convert = TRUE)
  fay <-
    stockassessment::faytable(fit) %>%
    as.data.frame() %>%
    dplyr::mutate(year = rownames(.) %>% as.integer()) %>%
    tidyr::gather(age, f, -year, convert = TRUE)
  res <-
    nay %>%
    dplyr::full_join(fay, by = c("year", "age")) %>%
    tibble::as_tibble()

  if(!missing(ibya)) {
    res %>%
      dplyr::full_join(ibya, by = c("year", "age")) %>%
      return()
  } else {
    res %>% return()
  }

}

#' sam_rby
#'
#' @description Get assessment summary data from "sam"-object
#'
#' @param fit XXX
#'
#' @return A tibble containing the following variables:
#' \itemize{
#'   \item year:
#'   \item est: Medium value
#'   \item low: Lower 2.5% quantile??
#'   \item high: Upper 2.5% quantile??
#'   \item variable: Name of variable (catch, recruitment, ssb, tsb and fbar)
#' }
#' @export
#'
sam_rby <- function(fit) {

  lh <- function(x, variable) {
    x %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var="year") %>%
      dplyr::mutate(year = as.integer(year),
                    variable = variable) %>%
      tibble::as_tibble()
  }

  dplyr::bind_rows(stockassessment::catchtable(fit) %>% lh("catch"),
                   stockassessment::rectable(fit)   %>% lh("rec"),
                   stockassessment::ssbtable(fit)   %>% lh("ssb"),
                   stockassessment::tsbtable(fit)   %>% lh("tsb"),
                   stockassessment::fbartable(fit)  %>% lh("fbar")) %>%
    dplyr::rename(est = Estimate,
                  low = Low,
                  high = High)

}

#' Title
#'
#' @param fit A "sam"-object
#'
#' @return A list containing tibbles "rbya" and "rby"
#' @export
#'
sam_rbx <- function(fit) {

  list(rbya = sam_rbya(fit, sam_ibya(fit)),
       rby = sam_rby(fit))

}

sam_rby_retro <- function(retro, ibya) {
  out <- list()
  for(i in 1:length(retro)) {
    out[[i]] <-
      sam_rby(retro[[i]]) %>%
      dplyr::mutate(assyear = i)
    bio <-
      stockassessment::ntable(retro[[i]]) %>%
      as.data.frame() %>%
      dplyr::mutate(year = rownames(.) %>% as.integer()) %>%
      tidyr::gather(age, n, -year, convert = TRUE) %>%
      dplyr::left_join(ibya %>% dplyr::select(year, age, cW)) %>%
      dplyr::filter(age %in% 4:14) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(bio = sum(n * cW) / 1e3)
    out[[i]] <-
      out[[i]] %>%
      dplyr::bind_rows(bio %>%
                         dplyr::select(year = year, Estimate = bio) %>%
                         dplyr::mutate(variable = "bio",
                                       assyear = i))

  }
  out %>% dplyr::bind_rows() %>% tibble::as_tibble()
}

sam_process_error <- function(rbya, plus_group=TRUE, plot_it=FALSE) {

  # dummy
  n.d <- cW <- year <- z.d <- age <- b <- 0

  # ----------------------------------------------------------------------------
  # align the year-classes
  x <- rbya[,c("year","age","n")]
  x$year <- x$year - 1
  x$age <- x$age - 1
  names(x)[3] <- "n.end"
  d <- plyr::join(rbya[,c("year", "age", "n", "m", "f", "cW")], x, by = c("year", "age"))
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
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE))
  x <- plyr::ddply(d,c("year"),plyr::summarise,b=sum(n.d * cW,na.rm=TRUE))

  if(plot_it) {
    mort <- ggplot2::ggplot(d,ggplot2::aes(year,z.d)) +
      ggplot2::theme_bw() +
      ggplot2::geom_text(ggplot2::aes(label=age)) +
      ggplot2::stat_smooth(span=0.1) +
      ggplot2::labs(x="",y="",title="Process error expressed as deviations in mortality")

    abun <- ggplot2::ggplot(d,ggplot2::aes(year,n.d)) +
      ggplot2::theme_bw() +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::facet_wrap(~ age, scale="free_y") +
      ggplot2::labs(x="", y="",title="Process error expressed as deviations in number of fish")

    mass <- ggplot2::ggplot(x[x$year < max(x$year),],ggplot2::aes(year,b)) +
      ggplot2::theme_bw() +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::labs(x="",y="Mass",title="Process error expressed as deviation in mass")

    return(list(rbya=d,rby=x,mort=mort,abun=abun,mass=mass))
  }

  return(list(rbya=d, rby=x))

}
