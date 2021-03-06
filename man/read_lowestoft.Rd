% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rw_lowestoft.R
\name{read_lowestoft}
\alias{read_lowestoft}
\title{read_lowestoft}
\usage{
read_lowestoft(file, sep = "", format = "long", quiet = TRUE, ...)
}
\arguments{
\item{file}{name of file, normally the index file name}

\item{sep}{the separator, default is ""}

\item{format}{character vector specifying output format: "long" (default),
"wide" or "list"
but can also use "list" which returns a list of matrices.}

\item{quiet}{boolean, default is TRUE}

\item{...}{other arguements}
}
\description{
Modified \href{https://github.com/flr/FLCore/blob/master/R/io.VPAsuite.R}{FLCore::readVPA}
function in the FLCore-package, except here it only returns a list or a data.frame
}
