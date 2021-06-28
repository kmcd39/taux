#' split_and_key
#'
#' Like [group_split] but uses grouping cols to form identifiers and uses these as
#' names for elements of split df. A broader purpose fcn that I may move elsewhere at
#' some point. Some surprise/discussion that this functionality isn't already built
#' into dplyr:
#' https://stackoverflow.com/questions/57107721/how-to-name-the-list-of-the-group-split-output-in-dplyr
#'
#' @param x object that inherits `data.frame`
#' @param cols a character vector of columns to split/groupby. Concatenation of these
#'   column names, separated by "-" will form the name for each resulting df after split.
#'
#' @export split_and_key
split_and_key <- function(x, .cols) {

  require(tidyverse)

  gx <- x %>%
    group_by(across(all_of(.cols)))

  out <- gx %>%
    group_split()

  names(out) <-
    gx %>%
    group_keys() %>%
    rowwise() %>%
    mutate(tags =
             glue::glue_collapse(c_across(), sep = "-")) %>%
    pull(tags)

  return(out)
}


#' rbind.key
#'
#'
#' Row binds a named list of tables, using element names to build a new identifier
#' column.
#'
#' @param x named list of data.frames to rbind
#'
#' @export rbind.key
rbind.key <- function(x, ids_to = "id", ...) {

  require(purrr)

  map2_dfr(x, names(x),
           ~tibble(!!rlang::sym(ids_to) := .y,
                   bind_rows(.x)))
}
