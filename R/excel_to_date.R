#' Convert Excel Garbage into an Actual Date
#'
#' @description Convert excel date codes into human (and R) readable dates.
#'
#' @param date `str/num` Date code to be converted
#' 
#' @examples
#' date <- 8573
#' excel_to_date(date)
#'
#' date <- '8573'
#' excel_to_date(date)
#'
#' @importFrom dplyr %>%
#' @export
excel_to_date <- function(date) {
  out <- date %>%
           as.numeric() %>%
           as.Date(origin = '1899-12-30')

  return(out)
}

