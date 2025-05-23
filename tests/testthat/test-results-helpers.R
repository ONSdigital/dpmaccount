## 'augment_population' -------------------------------------------------------

test_that("'augment_population' works with no collapsing", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- augment_population(x, n_draw = 5)
  expect_true(tibble::is_tibble(ans))
})

test_that("'augment_population' works with collapsing", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- augment_population(x, n_draw = 5, collapse = c("age", "sex", "cohort"))
  expect_true(tibble::is_tibble(ans))
})


## 'augment_events' -----------------------------------------------------------

test_that("'augment_events' works with no collapsing", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- augment_events(x, n_draw = 5)
  expect_true(tibble::is_tibble(ans))
})

test_that("'augment_events' works with collapsing", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- augment_events(x, n_draw = 5, collapse = c("age", "time"))
  expect_true(tibble::is_tibble(ans))
})


## 'augment_rates' ------------------------------------------------------------

test_that("'augment_rates' works with no collapsing", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- augment_rates(x, n_draw = 5)
  expect_true(tibble::is_tibble(ans))
})

test_that("'augment_rates' works with collapsing", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- augment_rates(x, n_draw = 5, collapse = "sex")
  expect_true(tibble::is_tibble(ans))
})
