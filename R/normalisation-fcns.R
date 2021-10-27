
#' normalize.to.range
#'
#' Normalizes a numeric vector `x` to between a given range
#'
#' @export normalize.to.range
normalize.to.range <- function(x, range = c(0, 1)) {

  (range[2] - range[1]) /
    (max(x) - min(x)) *
    (x - max(x)) + range[2]

}
