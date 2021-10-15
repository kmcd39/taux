
#' nas.by.colm
#'
#' Counts NAs for every column, or gets proportion of rows that are NA
#'
#' @export
nas.by.colm <- function(x, proportion = F) {

  if(!proportion)
    out <- x %>% map_dbl( ~sum(is.na(.x)) )
  else
    out <- x %>% map_dbl( ~sum(is.na(.x)) / length(.x) )

  return(out)
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
