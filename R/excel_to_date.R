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
#' @importFrom methods is
#' @export
excel_to_date <- function(date) {
  # double check that it wasn't already converted
  if (methods::is(date, 'Date')) {
    out <- date

  } else {
    out <- date %>%
             as.numeric() %>%
             as.Date(origin = '1899-12-30')
  }

  return(out)
}

