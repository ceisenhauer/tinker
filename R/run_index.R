#' Run Index
#'
#' @description provides the index of the first time a logical condition is satisfied a minimum of
#' n times in a row.
#' 
#' @param x `vector` vector to parse.
#' @param FUN `function` function indicating the condition to be satisfied.
#' @param n `integer` minimum run length (number of TRUE values in a row). default is 3.
#' @param direction `string` whether to parse x forward or backwards (resulting in the first or last
#'   time a run was observed respectively). default is forward.
#'
#' @return `int`
#'
#' @importFrom zoo rollsum
#' @importFrom tidyr replace_na
#' @importFrom purrr detect_index
#'
#' @export
run_index <- function(x, FUN, n = 3, direction = c('forward', 'backward')) {
  direction <- match.arg(direction)

  index <- x %>%
           sapply(FUN) %>%
           zoo::rollsum(k = n,
                        align = 'right',
                        na.pad = TRUE) %>%
           tidyr::replace_na(0) %>%
           purrr::detect_index(function(value) value == 4,
                              .dir = direction)

  if (direction == 'forward') {
    index <- index - n
  }

  return(index)
}

