#' IQR
#'
#' @description convenience function to extract the iqr of a vector as a formatted string (with or
#' without square brackets).
#' 
#' @param v `vector` vector to analyze.
#' @param digits `int` number of digits after the decimal to show in the iqr values.
#' @param percent `bool` whether to display values as percents
#' @param brackets `bool` if TRUE result will have square brackets (ie: [lower - upper] instead of
#'   lower - upper). default is FALSE.
#'
#' @return `str`
iqr <- function(v, digits = 1, percent = FALSE, brackets = FALSE) {
  quantiles <- quantile(v)

  lower <- round(quantiles[[2]], digits = digits)
  upper <- round(quantiles[[4]], digits = digits)

  iqr <- ifelse(percent, paste0(lower * 100, '% - ', upper * 100, '%'), paste(lower, '-', upper))
  iqr <- ifelse(brackets, paste0('[', iqr, ']'), iqr)

  return(iqr)
}

