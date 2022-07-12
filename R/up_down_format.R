#' Up / down formatting for a number
#' 
#' @description Creates an html string providing french format and color to x as well as adding an 
#' up / down arrow. By default, x will be red when positive, grey when 0, and blue when negative, 
#' but this can be inverted. Wrapping the text in parentheses is optional.
#' 
#' @param x `dbl` Number to be formatted.
#' @param flip_colors `bool` If TRUE, the default color scheme will be reversed such that negative 
#'   values are red and positive values are blue. Default is FALSE.
#' @param percent `bool` If TRUE, x will be interpreted as a proportion (ie 0.56) and displayed as a
#'   percent (ie. 56%). Default is TRUE.
#' @param decimals `int` Number of decimals to include in `x`. Default is 0. 
#' @param parentheses `bool` If TRUE, the text will be wrapped in parentheses. Default is TRUE.
#' 
#' @seealso [french_format()]
#' 
#' @return `chr`
#' 
#' @examples
#' x <- 5
#' y <- -10.76
#' 
#' up_down_format(x)
#' up_down_format(x,
#'                flip_colors = TRUE,
#'                parentheses = FALSE)
#' up_down_format(y,
#'                percent = FALSE,
#'                decimals = 1)
#' 
#' @export
up_down_format <- function(x, flip_colors = FALSE, 
                           percent = TRUE, decimals = 0,
                           parentheses = TRUE) {
  if (is.na(x)) {
    color <- '#807E7D'
  } else if (x == 0) {
    color <- '#807E7D'
  } else if (flip_colors) {
    color <- ifelse(x < 0, '#a80505', '#07008c')
  } else {
    color <- ifelse(x < 0, '#07008c', '#a80505')
  }

  arrow <- ifelse(x < 0, '\u25BC', '\u25B2')

  out <- paste0("<span style='color: ", color, "'>", 
                arrow, 
                french_format(abs(x),
                              decimals = decimals,
                              percent = percent),
                "</span>")

  if (parentheses) {
    out <- paste0('(', out, ')')
  }

  return(out)
}
