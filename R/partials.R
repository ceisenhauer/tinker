#' .Cut
#'
#' @description [cut] with sensible defaults for binning continuous variables : includes the lowest
#' value observed and closed on the left. By default the result will be ordered.
#'
#' @param ordered_result `bool` If `TRUE`, the output will be an ordered factor. Default is `TRUE`.
#' @param ... Additional arguments to be passed to [cut].
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
#' @description [IQR] that ignores NAs.
#'
#' @param ... Additional arguments to be passed to [IQR].
#'
#' @importFrom stats IQR
#'
#' @export
.IQR <- function(...) {
    stats::IQR(...,
               na.rm = TRUE)
}


#' .Max
#'
#' @description [max] that ignores NAs.
#'
#' @param ... Additional arguments to be passed to [max].
#'
#' @export
.max <- function(...) {
    max(...,
        na.rm = TRUE)
}


#' .Mean
#'
#' @description [mean] that ignores NAs.
#'
#' @param ... Additional arguments to be passed to [mean].
#'
#' @export
.mean <- function(...) {
    mean(...,
         na.rm = TRUE)
}


#' .Median
#' 
#' @description [median] that ignores NAs.
#'
#' @param ... Additional arguments to be passed to [median].
#' 
#' @importFrom stats median
#'
#' @export
.median <- function(...) {
    stats::median(...,
                  na.rm = TRUE)
}


#' .Min
#' 
#' @description [min] that ignores NAs.
#'
#' @param ... Additional arguments to be passed to [min].
#'
#' @export
.min <- function(...) {
    min(...,
        na.rm = TRUE)
}


#' .SD
#'
#' @description [sd] that ignores NAs.
#'
#' @param ... Additional arguments to be passed to [sd].
#'
#' @importFrom stats sd
#'
#' @export
.sd <- function(...) {
    stats::sd(...,
              na.rm = TRUE)
}


#' .Sum
#'
#' @description [sum] that ignores NAs.
#' 
#' @param ... Additional arguments to be passed to [sum].
#'
#' @export
.sum <- function(...) {
    sum(...,
        na.rm = TRUE)
}


#' .Summarize
#' 
#' @description Tidyverse [summarize] that drops groups by default.
#'
#' @param ... Additional arguments to be passed to [summarize].
#'
#' @importFrom dplyr summarize

#' @export
.summarize <- function(...) {
    dplyr::summarize(...,
                     .groups = 'drop')
}


#' .Quantile
#'
#' @description [quantile] that ignores NAs.
#'
#' @param ... Additional arguments to be passed to [quantile].
#'
#' @importFrom stats quantile
#'
#' @export
.quantile <- function(...) {
    stats::quantile(...,
                    na.rm = TRUE)
}

