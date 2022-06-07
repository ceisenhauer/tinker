validate_names <- function(df_names, df_new, fix_zone = NULL, fix_reg = NULL) {
  names <- df_names %>%
             select(reg, zone) %>%
             mutate(good = 'yes')

  df_new <- df_new %>%
           left_join(names)


  # identify failures)
  bad <- df_new %>%
           filter(is.na(good)) %>%
           select(reg, zone)


  # fix and update df of failures
  if (!is.null(fix_zone)) {
    df_new <- df_new %>%
                 mutate(zone = recode(zone, !!!fix_zone))
  }

  if (!is.null(fix_reg)) {
    df_new <- df_new %>%
                 mutate(reg = recode(reg, !!!fix_reg))
  }
    
  df_new <- df_new %>%
              select(-good) %>%
              left_join(names)

  bad <- df_new %>%
           filter(is.na(good)) %>%
           arrange(reg) %>%
           select(reg, zone)

  
  # generate warning if failures persist (just in case this function is reused on bigger data later)
  if (nrow(bad) > 0) {
    warning('some observations have incorrect zone / region names !')
    print(bad)
  }

  df_new <- df_new %>%
               select(-good)

  return(df_new)
}

