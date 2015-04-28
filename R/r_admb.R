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
#' @export
#' @param file Name of the file, normally ends with .hst
#' @param dir Name of the \emph{directory} that contains the result.
#' @param txt The parameters to extract
#' @param startyear Assessment start year
#' @param names A character vector of length two replacing the default names
#' \emph{c('value','prop')}
#' @param negative.allowed Flag, default is FALSE
#' @return A list, each component being a dataframe with two columns. The
#' default names are \emph{c('value','prop')} which can be replace by specifying
#' \emph{names}.
read_hst <- function (file, dir, txt, startyear, names, negative.allowed = FALSE)
{
  file <- paste(dir, file, sep = "/")
  tmpskra <- tempfile("bayes")
  on.exit(unlink(tmpskra))
  tmp <- scan(file, what = character(), sep = "\n")
  tmp1 <- matrix(tmp, length(tmp), 1)
  write.table(tmp1, file = tmpskra, sep = "", col.names = F,
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
    names(Result) <- paste("y", (startyear:(startyear + length(Result) - 1)),
                           sep = "")
    attributes(Result)$years <- startyear:(startyear + length(Result) -                                          1)
  }
  if (!missing(names)) names(Result) <- names
  if (length(Result) == 1) Result <- Result[[1]]
  return(Result)
}


#' Gets certain lines from file
#'
#' Some longer text here
#'
#' @param file Name of the file
#' @param line1 Integer, first line
#' @param line2 Integer, last line
#' @return A xxx
getlineswin <- function (file, line1, line2) {
  tmpskra <- tempfile("bayes")
  on.exit(unlink(tmpskra))
  #if (missing(nlines)) nlines <- length(count.fields(file, sep = "\n"))
  x <- scan(file, sep = "\t", what = character())
  x <- matrix(x, length(x), 1)
  x <- x[line1:line2, ]
  write.table(x, file = tmpskra, sep = "\n", col.names = F,
              row.names = F, quote = F)
  return(read.table(tmpskra))
}
