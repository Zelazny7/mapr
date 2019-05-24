library(mapr)
l <- spec_list(
  spec_clamp(-5, 9),
  spec_exception(-1, 3),
  spec_range(c(-1, 10), level = 1),
  spec_range(c(10, 20), level = 2),
  spec_missing(2)
  )

predict(l, -10:1)
