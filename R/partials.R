#' .Cut
#'
#' @description Cut with sensible defaults for binning continuous variables : includes the lowest
#' value observed and closed on the left. By default the result will be ordered.
#' 
#' @export
.cut <- function(ordered_result = TRUE, ...) {
    cut(...,
        include_lowest = TRUE,
        right = FALSE,
        ordered_result = ordered_result)
}


#' .IQR
#'
#' @description Max that ignores NAs.
#'
#' @export
.IQR <- function(...) {
    stats::IQR(...,
               na.rm = TRUE)
}


#' .Max
#'
#' @description Max that ignores NAs.
#'
#' @export
.max <- function(...) {
    stats::max(...,
               na.rm = TRUE)
}


#' .Mean
#'
#' @description Mean that ignores NAs.
#'
#' @export
.mean <- function(...) {
    mean(...,
         na.rm = TRUE)
}


#' .Median
#' 
#' @description Median that ignores NAs.
#' 
#' @export
.median <- function(...) {
    median(...,
           na.rm = TRUE)
}


#' .Min
#' 
#' @description Min that ignores NAs.
#' 
#' @export
.min <- function(...) {
    min(...,
        na.rm = TRUE)
}


#' .SD
#'
#' @description SD that ignores NAs.
#'
#' @export
.sd <- function(...) {
    sd(...,
       na.rm = TRUE)
}


#' .Summarize
#' 
#' @description Tidyverse summarize that drops groups by default.
#'
#' dplyr summarize
#' @export
.summarize <- function(...) {
    dplyr::summarize(...,
                     .groups = 'drop')
}


#' .Quantile
#'
#' @description Quantile that ignores NAs.
#'
#' @export
.quantile <- function(...) {
    quantile(...,
             na.rm = TRUE)
}
