test_that("provenance has core fields", {
  p <- capture_provenance()
  expect_true(is.character(p$r))
  expect_true(is.list(p$packages))
})


test_that("result + provenance have expected fields", {
  out <- dispatch("builtin", "score_move", list(state = list(x = 0L)),
                  registry = builtin_registry)
  prov <- capture_provenance(list(latencyMs = 10L))
  expect_true(is.list(out))
  expect_true(is.numeric(out$score))
  expect_true(is.list(out$meta))
  expect_true(is.character(prov$r))
  expect_true(is.numeric(prov$latencyMs))
})
