test_that("compose_registry returns builtin by default", {
  reg <- compose_registry()
  expect_true(is.list(reg))
  expect_true(is.function(reg$builtin$score_move))
})
