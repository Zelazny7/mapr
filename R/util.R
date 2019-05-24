#' Iter Pairs
#' Iterate over subsequent pairs of elements
#' @param vec A vector to be iter-paired
#' @return A list of 2-element lists
#' @details iter_pairs returns a list of adjacent pairs. Elements
#' from the original list paired like so: (1,2), (2,3), ..., (n-1, n)
iter_pairs <- function(vec) {
  stopifnot(is.vector(vec))
  mapply(list, head(vec, -1), tail(vec, -1), SIMPLIFY = F)
}
