% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source_directory.R
\name{source_directory}
\alias{source_directory}
\title{Source Directory}
\usage{
source_directory(path_to_directory = ".", recursive = FALSE)
}
\arguments{
\item{path_to_directory}{\code{string/vector} String or vector of strings specifying directory to be
sourced. Path name should not include a trailing '/'}

\item{recursive}{\code{bool} Whether to source files recursively (in directory and subdirectories).
default is FALSE.}
}
\value{
NONE
}
\description{
Source all files in a specified directory or set of directories.
}
\examples{
\dontrun{
  source_directory('code/functions')
}

}
