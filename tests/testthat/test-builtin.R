test_that("builtin_registry has expected shape", {
  expect_true(is.list(builtin_registry))
  expect_true(is.function(builtin_registry$builtin$score_move))
})
