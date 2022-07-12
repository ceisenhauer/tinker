#' Convert french-style number string to numeric
#' 
#' @description Takes a french style string to a numberic, for example `"1 001,5"` becomes `1001.5`.
#' 
#' @param str `chr` String to convert.
#' 
#' @examples
#' french_to_numeric("1 001,5")
#' 
#' @importFrom dplyr %>%
#' @export
french_to_numeric <- function(str) {
  out <- str %>%
    sub(',', '.', .) %>%
    sub(' ', '', .) %>%
    as.numeric()

  return(out)
}
