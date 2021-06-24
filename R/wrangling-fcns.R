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


#' named_map_dfr
#'
#' Uses names from a named list in an identifier column alongside other output in
#' map_dfr
#'
#' @export
named_map_dfr <- function(.x, .f, ...) {

  require(purrr)

  map(.x, .f, ...) %>%
    map2_dfr(., names(.),
           ~tibble(div.type = .y,
                   bind_rows(.x)))
}
