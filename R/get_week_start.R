#' Start of Week
#'
#' @description Gets the first monday of a given ISO week-year.
#'
#' @param week `int/str` ISO week number, zero padding is accepted but not required.
#' @param year `int/str` ISO year. Must be formatted as YYYY. 
#'
#' @importFrom stringr str_pad
#' @importFrom ISOweek ISOweek2date
get_week_start <- function(week, year) {
  if (is.na(week) | is.na(year)) {
    date <- NA
  } else {
    week <- stringr::str_pad(week, 2, pad = '0')
    date <- ISOweek::ISOweek2date(paste0(year, '-W', week, '-1'))
  }

  return(date)
}
