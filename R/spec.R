#' @include validation_funcs.R
NULL

#' Spec constructor
#' @param x a spec sub-type
#' @param level relative predicted ordering
spec <- function(x, level) {
  x$level <- level
  class(x) <- c("spec", class(x))
  x
}

#' Exception Spec
#' @param value single value to be constrained
#' @param level relative predicted ordering
#' @export
spec_exception <- function(value, level) {
  out <- structure(
    list(value=value),
    class=c("spec_exception"))
  spec(out, level)
}

#' Missing Spec
#' @param level relative predicted ordering
#' @export
spec_missing <- function(level) {
  spec(structure(list(), class="spec_missing"), level)
}

#' Range Spec
#' @param rng length-2 numeric vector specifying interval
#' @param linc boolean left boundary inclusive or exclusive
#' @param rinc boolean right boundary inclusive or exclusive
#' @param mono monotonicity constraint {-1, 0, 1}
#' @param level relative predicted ordering
#' @export
spec_range <- function(rng=c(-Inf, Inf), linc=TRUE, rinc=FALSE, mono=0, level) {
  stopifnot(identical(length(rng), 2L))
  out <- structure(
    list(rng=rng, linc=linc, rinc=rinc, mono=mono),
    class="spec_range")
  spec(out, level)
}

#' Clamp Spec
#' @param ll lower limit
#' @param ul  upper limit
#' @details Unlike other specs, the clamp spec does not
#' have a relative ordering. It only limits the range of a
#' continuous variable to be within a certain upper and lower
#' limit.
#' @export
spec_clamp <- function(ll=-Inf, ul=Inf) {
  stopifnot(ul > ll)
  spec(structure(list(ll=ll, ul=ul), class=c("spec_clamp")), NULL)
}


#' Spec Priority
#' @param x A spec object
#' @details Specifications must be processed in a certain order
#' otherwise transformed variables may be overwritten. The processing
#' of specs done in the following order:
#' \enumerate{
#' \item{Missing}
#' \item{Exceptions}
#' \item{Clamp}
#' \item{Range sorted by ascending lower boundary value}
#' }
#' @export
priority <- function(x) UseMethod("priority")

#' @export
priority.spec_missing <- function(x) c(1, -Inf)

#' @export
priority.spec_exception <- function(x) c(2, x$value)

#' @export
priority.spec_clamp <- function(x) c(3, -Inf)

#' @export
priority.spec_range <- function(x) c(4, x$rng[[1]])

sort_spec_list <- function(sl) {
  # sort elements based on function
  p <- t(sapply(sl, priority))
  i <- order(p[,1], p[,2])
  sl[i]
}

#' Spec List
#' @param ... spec objects
#' @export
spec_list <- function(...) {
  dots <- list(...)

  # validation
  check_all_spec(dots)
  check_single_types(dots)
  check_range_overlap(dots)

  structure(
    list(
      specs=sort_spec_list(dots),
      sorted=TRUE),
    class = c("spec", "spec_list"))
}

#' @export
predict.spec_missing <- function(s, x, level, f=FALSE) {
  f <- !f & is.na(x)
  x[f] <- level
  list(x, f)
}

lt <- function(linc) if (linc) `>=` else `>`
gt <- function(rinc) if (rinc) `<=` else `<`

#' @export
predict.spec_range <- function(s, x, level, f=FALSE) {
  ll <- lt(s$linc)(x, s$rng[[1]])
  ul <- gt(s$rinc)(x, s$rng[[2]])
  f <- !f & ll & ul
  x[f] <- level
  list(x, f)
}

#' @export
predict.spec_exception <- function(s, x, level, f=FALSE) {
  f <- !f & x == s$value
  x[f] <- level
  list(x, f)
}

#' @export
predict.spec_clamp <- function(s, x, i, f=FALSE) {
  list(pmin(pmax(x, s$ll), s$ul), f)
}

#' @export
predict.spec_list <- function(sl, x) {
  stopifnot(sl$sorted)
  f <- FALSE
  for (i in seq_along(sl$specs)) {
    res <- predict(sl$specs[[i]], x, i, f)
    x <- res[[1]]
    f <- res[[2]]
  }
  x
}
