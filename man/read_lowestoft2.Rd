% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rw_lowestoft.R
\name{read_lowestoft2}
\alias{read_lowestoft2}
\title{read_lowestoft_file}
\usage{
read_lowestoft2(
  file,
  format = "long",
  value.name = "x",
  sep = "",
  quiet = TRUE
)
}
\arguments{
\item{file}{name of file, normally the index file name}

\item{format}{output format, "matrix","wide","long" (default)}

\item{value.name}{Name of value, only relevant for format="long"}

\item{sep}{the separator, default is ""}

\item{quiet}{boolean, default is TRUE}
}
\description{
The code is inpired by the
 \href{https://github.com/flr/FLCore/blob/master/R/io.VPAsuite.R}{FLCore::readVPAFile} function.
The difference is that it is not dependent on the FLCore, including the S4-methods.
}
