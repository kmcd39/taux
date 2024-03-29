
#' colm.in.tblList
#'
#' Given a list of data.frames, find which ones have a column that match a regex
#' string.
#'
#' @param xL list of data.frames
#' @param regex regular expression
#' @param return.tbls whether to return tables (default) or names of tables
#'   containing column (or index to those tables if unnamed `xL`)
#'
#' @export colm.in.tblList
colm.in.tblList <- function(xL, regex, return.tbls = F) {

  index <-
    purrr::map_lgl( xL,
                    ~any(grepl(regex, colnames(.x)))
    )

  if(!return.tbls){

    if(!is.null(names(index)))
      return(names(index)[index])
    else
      return(index)
  }

  xL[index]
}



#' split_and_key
#'
#' Like [group_split] but uses grouping cols to form identifiers and uses these as
#' names for elements of split df. A broader purpose fcn that I may move elsewhere at
#' some point. Some surprise/discussion that this functionality isn't already built
#' into dplyr:
#' https://stackoverflow.com/questions/57107721/how-to-name-the-list-of-the-group-split-output-in-dplyr
#'
#' Reminder that a nest/unnest workflow is often better in most cases, rather than
#' splitting
#'
#' @param x object that inherits `data.frame`
#' @param cols a character vector of columns to split/groupby. Concatenation of these
#'   column names, separated by "-" will form the name for each resulting df after
#'   split.
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
#' @param ids_to name of id column in new data.frame
#'
#' @export rbind.key
rbind.key <- function(x, ids_to = "id") {

  require(purrr)

  map2_dfr(x, names(x),
           ~tibble(!!rlang::sym(ids_to) := .y,
                   bind_rows(.x)))
}




#' read.tbList
#'
#' Read and name a tablelist, given a directory containing multiple csvs
#'
#' @param ddir directory containing tables to read.
#' @param pattern,delim passed onto `list.files` and `vroom::vroom`, respectively
#'
#' @export read.tbList
read.tbList <- function(ddir
                        , pattern = 'csv$'
                        , delim = ',') {

  # filenames in directory matching pattern
  fns <- list.files(ddir
                    , pattern = pattern)
  # read all from fns
  xL <- map( paste0(ddir, fns)
             , ~{ print(.x)
               vroom::vroom(.x
                            ,delim=delim)})

  # drop file extension to make table name
  nms <- gsub('\\.csv$', '', fns)
  names(xL) <- nms

  return(xL)
}
