% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sam.R
\name{sam_opr}
\alias{sam_opr}
\title{Sam fit}
\usage{
sam_opr(fit, scale = 1, run)
}
\arguments{
\item{fit}{A "sam" object}

\item{scale}{A scale for the scaleable variables}

\item{run}{Name of the run}
}
\value{
A list of length 2 containing the following:
\itemize{
   \item data: A tibble with the following variables:
   \itemize{
      \item year:
      \item age:
      \item fleet: Name of the fleets
      \item o: Observed value, default log scale
      \item p: Predicted value, default log scale
      \item r: Residuals, not yet returned
      }
   }
}
\description{
Observed and predicted values
}
