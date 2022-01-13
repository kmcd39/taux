
#' sum.NAs
#'
#' Counts NAs for every column, or gets proportion of rows that are NA
#'
#' @import tidyverse
#'
#' @export sum.NAs
sum.NAs <- function(x, proportion = F) {

  if(!proportion)
    out <- x %>% purrr::map_dbl( ~sum(is.na(.x)) )
  else
    out <- x %>% purrr::map_dbl( ~sum(is.na(.x)) / length(.x) )

  return(out)
}

#' count.across
#'
#' Applies the `count` fcn across all or a selection of columns in a data.frame.
#'
#' @param x data.frame
#' @param .cols tidy selector for given columns. By default, selects all of them.
#'
#' @export count.across
count.across <- function(x, .cols = everything()) {

  x <- x %>% select(.cols)

  colnames(x) %>%
    purrr::map( ~count(x,
                       !!rlang::sym(.)) ) %>%
    purrr::map( ~arrange(., desc(n)) )

}



#' quantiles.across.groups
#'
#' Gets (wide) quantiles by group, so each group remains one row.
#'
#' Could improve by adding tdiy-style quotation:
#' https://adv-r.hadley.nz/quasiquotation.html
#'
#'
#' @param gx grouped `data.frame` or class that inherits
#' @param .colm column to get quantiles for, as string
#'
#' @export quantiles.across.groups
quantiles.across.groups <- function(gx,
                                    .colm,
                                    ntile = .25) {

  .colm <- rlang::sym(.colm)
  # quartiles across models
  gx %>%
    summarise(
      bucket = names(
        quantile(!!.colm, seq(0,1, ntile))
      ),
      quartile = quantile(!!.colm, seq(0,1, ntile))) %>%
    ungroup() %>%
    pivot_wider(values_from = "quartile",
                names_from = "bucket")
}

