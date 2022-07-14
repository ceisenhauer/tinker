#' ISO Week-Year to Date
#'
#' @description Provides the date associated with a given ISO year - week number - day of week.
#'
#' @param year `int` Year (with %Y% format). Default is the current year.
#' @param week `int` ISO week number (%V). Must be between 1 and 53. A warning will be provided if 
#'   `week` is 53 but `year` is not a leap year. Default is the current week.
#' @param day `int` ISO week day. Must be between 1 and 7 where 1 is Monday and 7 is Sunday. Default
#'   is 1 (Monday).
#' 
#' @export
iso_to_date <- function(year = format(Sys.Date(), '%Y%'), week = format(Sys.Date(), '%V'),
                        day = 1) {
  # TODO: vectorize
  ## validate that week and day are ok
  #if (week > 53 || week < 1) {
    #stop(paste0('week number out of bounds, expected 1 - 53 but found ', week))
  #}

  #if (day > 8 || day < 1) {
    #stop(paste0('day number out of bounds, expected 1 - 7 but found ', week))
  #}
  
  # get first iso date of the year
  jan4 <- as.Date(ifelse(is.na(year), NA, paste0(year, '-', 1, '-', 4)))
  first_monday <- jan4 - as.numeric(format(jan4, '%u')) + 1

  # zero index week and day then find final date
  week <- week - 1
  day <- day - 1

  out <- first_monday + 7 * week + day

  # TODO: vectorize
  ## check for leap year issues
  #if (week == 52) {
    #out_day_of_month <- as.numeric(format(out, '%d'))
    #out_year <- as.numeric(format(out, '%Y'))

    #if (out_day_of_month < 4 & out_year > year) {
      #warning(paste0('careful, week = 53 but ', year, ' is not a leap year...'))
    #}
  #}

  return(out)
}

