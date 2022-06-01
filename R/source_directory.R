#' Source Directory
#'
#' @description Source all files in a specified directory or set of directories.
#' 
#' @param path_to_directory `string/vector` String or vector of strings specifying directory to be
#'   sourced. Path name should not include a trailing '/'
#' @param recursive `bool` Whether to source files recursively (in directory and subdirectories).
#'   default is FALSE.
#'
#' @return NONE
#' 
#' @examples
#' \dontrun{
#'   source_directory('code/functions')
#' }
#'
#' @export
source_directory <- function(path_to_directory = '.', recursive = FALSE) {
  files <- list.files(path_to_directory,
                      pattern = '.R$',
                      full.names = TRUE,
                      ignore.case = TRUE,
                      recursive = recursive)

  sapply(files,
         source,
         .GlobalEnv)
}

