#' @export
print.spec <- function(x, ...) cat(toString(x), sep = "")

toString.spec_clamp <- function(x, collapse = ", "){
  sprintf("{%.2f, %.2f}", x$ll, x$ul)
}

toString.spec_range <- function(x, collapse = ", ") {
  lb <- if (x$linc) "[" else "("
  ub <- if (x$rinc) "]" else ")"
  sprintf("%s%.2f, %.2f%s", lb, x$rng[[1]], x$rng[[2]], ub)
}

toString.spec_missing <- function(x, collapse = ", ") "Missing"

toString.spec_exception <- function(x, collapse = ", ") {
  as.character(x$value)
}

toString.spec_list <- function(x, collapse = "\n") {
  do.call(paste, c(lapply(x$specs, toString), sep="\n"))
}
