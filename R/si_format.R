#' SI prefix notation for large numers
#' 
#' @description Applies SI prefix notation to a numer, `x`. This is intended for epidemic estimates
#' and works for k and M only.
#' 
#' @param x `dbl / int` Numer to be abreviated.
#' 
#' @return `chr`
#' 
#' @examples 
#' x <- 123456
#' si_format(x)
#' 
#' y <- 12345678
#' si_format(y)
#' 
#' @export
si_format <- function(x) {
  if (is.na(x)) {
    return(x)
    
  } else if (x >= 1e6) {
    if (x %% 1e6 != 0  & x < 1e7 ) {
      x <- sprintf('%1.1fM', x / 1e6) %>% sub('\\.', ',', .)
      return(x)
    }

    x <- sprintf('%1.0fM', x / 1e6)
    return(x)

  } else if (x >= 1e3) {
    if (x %% 1e3 != 0 & x < 1e4) {
      x <- sprintf('%1.1fk', x / 1e3) %>% sub('\\.', ',', .)
      return(x)
    }

    x <- sprintf('%1.0fk', x / 1e3)
    return(x)
  }

  return(x)
}

