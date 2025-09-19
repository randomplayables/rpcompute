test_that("builtin score_move works", {
  reg <- builtin_registry
  out <- dispatch("builtin", "score_move", list(state = list(x = 5L)), registry = reg)
  expect_equal(out$score, 12L)
  expect_true(is.list(out$meta))
})


test_that("dispatch errors on unknown mode/fn", {
  reg <- builtin_registry
  expect_error(dispatch("nope", "score_move", registry = reg), "Unknown mode")
  expect_error(dispatch("builtin", "nope", registry = reg), "Unknown function")
})

test_that("dispatch honors seed for determinism", {
  reg <- list(builtin = list(rand = function(args) list(x = runif(1))))
  a <- dispatch("builtin", "rand", seed = 123, registry = reg)
  b <- dispatch("builtin", "rand", seed = 123, registry = reg)
  c <- dispatch("builtin", "rand", seed = 124, registry = reg)
  expect_equal(a$x, b$x)
  expect_false(identical(a$x, c$x))
})
