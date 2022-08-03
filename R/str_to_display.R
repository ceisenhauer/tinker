#' Create Display Version of String
#'
#' @description Creates a display version of a string by replacing underscores with spaces and 
#' applying title case.
#'
#' @param text `str` Text to be converted.
#' 
#' @examples
#' text <- 'bas_uele'
#' str_to_display(text)
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace str_to_title
#' @export
str_to_display <- function(text) {
  out <- text %>%
           stringr::str_replace('_', ' ') %>%
           stringr::str_to_title()
}

