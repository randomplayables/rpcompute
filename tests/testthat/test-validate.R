test_that("validate_payload passes and fails as expected", {
  expect_invisible(validate_payload(list(mode = "builtin", fn = "score_move")))
  expect_error(validate_payload(list(fn = "x")), "Missing")
  expect_error(validate_payload(list(mode = "builtin")), "Missing")
  expect_error(validate_payload("not a list"))
})
