% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/french_format.R
\name{french_format}
\alias{french_format}
\title{French (numeric) formatter}
\usage{
french_format(x, decimals = 0, percent = FALSE)
}
\arguments{
\item{x}{\code{int / dbl} Number to be formatted.}

\item{decimals}{\code{int} Number of decimal values to include. Default is 0.}

\item{percent}{\code{bool} Whether to format the number as a percent. \emph{Warning}: \code{french_format()}
expects a \emph{proportion} (ie 0.72) not a \emph{percent} (ie 72). Default is FALSE.}
}
\value{
\code{chr}
}
\description{
Convenience function that converts a number (int or dbl) to have a french format
with a space as the big mark and a comma as the decimal mark. Optional percent formatting is
available; note that this case expects a \emph{proportion} as the input (ie to yeild 72\%, you will
pass 0.72 not 72).
}
\examples{
val <- 10 / 3
french_format(val)
french_format(val,
              decimals = 2)
french_format(val,
              decimals = 1,
              percent = TRUE)

}
