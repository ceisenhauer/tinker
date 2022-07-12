#' French (numeric) formatter
#' 
#' @description Convenience function that converts a number (int or dbl) to have a french format
#' with a space as the big mark and a comma as the decimal mark. Optional percent formatting is 
#' available; note that this case expects a *proportion* as the input (ie to yeild 72%, you will 
#' pass 0.72 not 72).
#' 
#' @param x `int / dbl` Number to be formatted.
#' @param decimals `int` Number of decimal values to include. Default is 0.
#' @param percent `bool` Whether to format the number as a percent. *Warning*: `french_format()` 
#'   expects a *proportion* (ie 0.72) not a *percent* (ie 72). Default is FALSE.
#' 
#' @return `chr`
#' 
#' @examples
#' val <- 10 / 3
#' french_format(val)
#' french_format(val,
#'               decimals = 2)
#' french_format(val,
#'               decimals = 1,
#'               percent = TRUE)
#' 
#' @export
french_format <- function(x, decimals = 0, percent = FALSE) {
  x <- ifelse(percent, x * 100, x)

  out <- formatC(x,
                 format = 'f',
                 big.mark = ' ',
                 decimal.mark = ',',
                 digits = decimals)

  out <- ifelse(percent, paste0(out, "%"), out)

  return(out)
}

