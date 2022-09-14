#' Validate Names
#'
#' @description Utility function to assist name validation / fixing. The function will update names
#' with a user provided list at both the zone (smaller) and region (larger) levels if provided. It
#' will also print a list of all remaining unmatched / invalid names. **WARNING** : this function is
#' *opinionated* and assumes that both `df_new` and `df_ref` have columns called "zone" and "reg"; 
#' if those columns are not present the function will fail.
#'
#' @param df_new `dataframe` Dataframe with names to be validated / fixed.
#' @param df_ref `dataframe` Reference dataframe containing the accepted names you want to match to.
#' @param fix_zone `list` Named list of correct zone names (where zone is the smaller geographic 
#'   level). If  provided, this list will be used to update `df_new` and must have the format where
#'   the list  names (keys) match what is in df_new and the list values are the updated (correct)
#'   names. The default is `NULL`.
#' @param fix_reg `list` Named list of correct region names (where region is the larger geographic
#'   level). Equivalent of `fix_zone` but at the region level. Default is `NULL`.
#'
#' @importFrom dplyr select mutate left_join filter recode arrange
#' @importFrom rlang .data
#'
#' @export
validate_names <- function(df_new, df_ref, fix_zone = NULL, fix_reg = NULL) {
  names <- df_ref %>%
             dplyr::select(.data$reg, .data$zone) %>%
             dplyr::distinct() %>%
             dplyr::mutate(good = 'yes')

  df_new <- df_new %>%
           dplyr::left_join(names)


  # identify failures)
  bad <- df_new %>%
           dplyr::filter(is.na(.data$good)) %>%
           dplyr::select(.data$reg, .data$zone)


  # fix and update df of failures
  if (!is.null(fix_zone)) {
    df_new <- df_new %>%
                 dplyr::mutate(zone = dplyr::recode(.data$zone, !!!fix_zone))
  }

  if (!is.null(fix_reg)) {
    df_new <- df_new %>%
                 dplyr::mutate(reg = dplyr::recode(.data$reg, !!!fix_reg))
  }
    
  df_new <- df_new %>%
              dplyr::select(-.data$good) %>%
              dplyr::left_join(names)

  bad <- df_new %>%
           dplyr::filter(is.na(.data$good)) %>%
           dplyr::arrange(.data$reg) %>%
           dplyr::select(.data$reg, .data$zone)

  
  # generate warning if failures persist (just in case this function is reused on bigger data later)
  if (nrow(bad) > 0) {
    warning('some observations have incorrect zone / region names !')
    print(as.data.frame(bad))
  }

  df_new <- df_new %>%
               dplyr::select(-.data$good)

  return(df_new)
}

