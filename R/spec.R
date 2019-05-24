## create vectors that enforce constraints.. simpler

## need to support capping as well

## simple code generation

## specify a range, monotonicity, and level, clamp

spec <- function(x, level) {
  x$level <- level
  class(x) <- c("spec", class(x))
  x
}

spec_exception <- function(value, level) {
  out <- structure(
    list(value=value),
    class=c("spec_exception"))
  spec(out, level)
}

spec_missing <- function(level) {
  spec(structure(list(), class="spec_missing"), level)
}

spec_range <- function(rng, linc=TRUE, rinc=FALSE, mono=0, level) {
  out <- structure(
    list(rng=rng, linc=linc, rinc=rinc, mono=mono),
    class="spec_range")
  spec(out, level)
}

spec_clamp <- function(ll=-Inf, ul=Inf) {
  stopifnot(ul > ll)
  spec(structure(list(ll=ll, ul=ul), class=c("spec_clamp")), NULL)
}


priority <- function(x) UseMethod("priority")
priority.spec_missing <- function(x) c(1, -Inf)
priority.spec_exception <- function(x) c(2, x$value)
priority.spec_clamp <- function(x) c(3, -Inf)
priority.spec_range <- function(x) c(4, x$rng[[1]])


# list of specs
spec_list <- function(...) {
  dots <- list(...)

  # validation
  check_all_spec(dots)
  check_single_types(dots)
  check_range_overlap(dots)

  # sort elements based on function
  p <- t(sapply(dots, priority))
  i <- order(p[,1], p[,2])

  structure(
    dots[i],
    class = c("spec", "spec_list"))
}


predict.spec_missing <- function(s, x, level, f=FALSE) {
  f <- !f & is.na(x)
  x[f] <- level
  list(x, f)
}

lt <- function(linc) if (linc) `>=` else `>`
gt <- function(rinc) if (rinc) `<=` else `<`

predict.spec_range <- function(s, x, level, f=FALSE) {
  #browser()
  ll <- lt(s$linc)(x, s$rng[[1]])
  ul <- gt(s$rinc)(x, s$rng[[2]])
  f <- !f & ll & ul
  x[f] <- level
  list(x, f)
}

predict.spec_exception <- function(s, x, level, f=FALSE) {
  f <- !f & x == s$value
  x[f] <- level
  list(x, f)
}

predict.spec_clamp <- function(s, x, i, f=FALSE) {
  list(pmin(pmax(x, s$ll), s$ul), f)
}

predict.spec_list <- function(sl, x) {
  f <- FALSE
  for (i in seq_along(sl)) {
    res <- predict(sl[[i]], x, i, f)
    x <- res[[1]]
    f <- res[[2]]
  }
  x
}
