#' Not in operator
#' 
#' @description Convenience operator inverting the functionality of `%in%`; ie, x %notin% y will 
#' return a bool (or vector of bools) indicating whether x (or its members) are in y.
#' 
#' @param x `value / list` Value(s) to look for in `y`.
#' @param y `list` List of values.
#' 
#' @return `%notin%` operator
#' 
#' @examples
#' \dontrun{
#'   test_strings <- c('hello', 'world')
#'   c('foobar', 'hello') %notin% test_strings
#' }
#' 
#' @export
'%notin%' <- function(x, y) {
  !('%in%'(x, y))
}

