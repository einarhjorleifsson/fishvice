#' @title Reads prelude files
#'
#' @description Read data files in
#' \href{http://www.hafro.is/~gunnar/manual/pdf/080_Forleikur.pdf}{prelude format},
#' which has 2 line headers, the first containing the column name the second
#' containing dashse.
#'
#' Based on script in the geo-package, there called s2pre.
#'
#' @export
#' @param file the name of the file which the data are to be read from.
#' @param rownames a logical value (default is FALSE) indicating whether the
#' first column should be treated as row names.
#' @param underscore2dot a logical value (default is FALSE) indicating if
#' underscores in column names should be replaced with dots. Note the default is
#' the reverse from that in the function s2pre, the latter being a somewhat odd
#' default.
#'
#' @author hoski@@hafro.is
#'
#' @seealso \code{\link{write_prelude}} for writing prelude files
#'
read_prelude <- function (file, rownames = FALSE, underscore2dot = FALSE)
{

  if(!file.exists(file)) {
    message(paste(file,"does not exist"))
    return(NULL)
  }

  fields <- utils::count.fields(file, sep = "\t")

  nrec <- length(fields)
  if (nrec == 2) return(NULL)

  collab <- scan(file = file, what = character(), sep = "\t",
                 n = fields[1],quiet=TRUE)
  outp <- utils::read.table(file, sep = "\t", skip = 2, as.is = T,
                     row.names = NULL, na.strings = "")
  names(outp) <- collab
  if (rownames) {
    row.names(outp) <- outp[, 1]
    outp <- outp[, 2:ncol(outp)]
  }
  if (underscore2dot)
    names(outp) <- stringr::str_replace_all(names(outp))
  return(outp)
}

#' @title Writes prelude file
#'
#' @description Writes \code{data.frame} or \code{matrix} objects to a file
#' in
#' \href{http://www.hafro.is/~gunnar/manual/pdf/080_Forleikur.pdf}{prelude format}.
#'
#' @export
#' @param data the object to be written, a \code{matrix} or \code{data.frame}.
#' @param file a character string for the output file name (default is "R.pre").
#' @param na.replace A character to replace NA with in the output file ("" by default)
#'
#' @note If file exist it is simply overwritten without any warning
#'
#' @author hoski@@hafro.is
#'
#' @seealso \code{\link{read_prelude}} for reading prelude files
write_prelude <- function (data, file = "R.pre", na.replace = "")
{
  if (is.data.frame(data))
    data <- as.matrix.data.frame(data)
  data[is.na(data) | data == "NA"] <- na.replace
  col.names <- dimnames(data)[[2]]
  if (is.null(col.names) || length(col.names) == 0)
    col.names <- paste("dalkur", 1:ncol(data), sep = "")
  row.names <- dimnames(data)[[1]]
  if (!is.null(row.names) && length(row.names) > 0) {
    col.names <- c("linu_nofn", col.names)
    data <- cbind(row.names, data)
  }
  n.of.col <- length(col.names)
  cat(col.names, sep = c(rep("\t", n.of.col - 1), "\n"), file = file)
  strika.lina <- rep("", n.of.col)
  for (i in 1:n.of.col) strika.lina[i] <- paste(rep("-", nchar(col.names[i])),
                                                collapse = "")
  cat(strika.lina, sep = c(rep("\t", n.of.col - 1), "\n"),
      file = file, append = T)
  cat(t(data), sep = c(rep("\t", n.of.col - 1), "\n"), file = file,
      append = T)
  return(invisible(NULL))
}

