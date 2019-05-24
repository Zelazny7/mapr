
is.spec_missing <- function(x) inherits(x, "spec_missing")
is.spec_exception <- function(x) inherits(x, "spec_exception")
is.spec_range <- function(x) inherits(x, "spec_range")
is.spec_clamp <- function(x) inherits(x, "spec_clamp")
is.spec <- function(x) inherits(x, "spec")

check_all_spec <- function(l) {
  if (!all(sapply(l, is.spec))) {
    stop("All objects passed to ... must be `spec` objects")
  }
}

check_single_types <- function(l) {
  if (length(Filter(is.spec_missing, l)) > 1) {
    stop("Only one missing spec can be passed to spec_list")
  }
  if (length(Filter(is.spec_clamp, l)) > 1) {
    stop("Only one clamp spec can be passed to spec_list")
  }
}

sort_ranges <- function(l) {
  ll <- vapply(l, function(x) x$rng[[1]], numeric(1))
  l[order(ll)]
}

check_range_overlap <- function(l) {
  r <- sort_ranges(Filter(is.spec_range, l))
  if (length(r) > 1) {
    pairs <- iter_pairs(r)
    for (pair in pairs) {
      if (((pair[[1]]$rng[[2]] == pair[[2]]$rng[[1]]) &&
           (pair[[1]]$rinc == pair[[2]]$linc)) ||
          (pair[[1]]$rng[[2]] != pair[[2]]$rng[[1]]) ||
          (pair[[1]]$rng[[2]] > pair[[2]]$rng[[1]]))
        stop("Disjoint or overlapping range specification: ",
             toString(pair[[1]]), " ",
             toString(pair[[2]]))
    }
  }
}
