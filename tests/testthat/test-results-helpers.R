## 'augment_population' -------------------------------------------------------

test_that("'augment_population' works with no collapsing", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  x <- do.call(estimate_account, args)
  ans <- augment_population(x, n_draw = 5)
  expect_true(tibble::is_tibble(ans))
})

test_that("'augment_population' works with collapsing", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  x <- do.call(estimate_account, args)
  ans <- augment_population(x, n_draw = 5, collapse = c("age", "sex", "cohort"))
  expect_true(tibble::is_tibble(ans))
})


## 'augment_events' -----------------------------------------------------------

test_that("'augment_events' works with no collapsing", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  x <- do.call(estimate_account, args)
  ans <- augment_events(x, n_draw = 5)
  expect_true(tibble::is_tibble(ans))
})

test_that("'augment_events' works with collapsing", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  x <- do.call(estimate_account, args)
  ans <- augment_events(x, n_draw = 5, collapse = c("age", "time"))
  expect_true(tibble::is_tibble(ans))
})


## 'augment_rates' ------------------------------------------------------------

test_that("'augment_rates' works with no collapsing", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  x <- do.call(estimate_account, args)
  ans <- augment_rates(x, n_draw = 5)
  expect_true(tibble::is_tibble(ans))
})

test_that("'augment_rates' works with collapsing", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  x <- do.call(estimate_account, args)
  ans <- augment_rates(x, n_draw = 5, collapse = "sex")
  expect_true(tibble::is_tibble(ans))
})

## 'increments' ---------------------------------------------------------------

test_that(" 'increments' works with valid inputs", {
  args <- list(datamods = sim_arg_data_models(), sysmods = sim_arg_system_models(), seed_in = 0)
  object <- do.call(estimate_account, args)

  cohort <- object$cohort
  sex <- object$sex
  fitted <- object$fitted
  data <- lapply(fitted, increments_comod)
  n <- vapply(data, nrow, 0L)
  cohort <- rep(cohort, times = n)
  sex <- rep(sex, times = n)
  data <- do.call(rbind, data)
  ans_expected <- tibble::tibble(
    cohort,
    sex,
    data
  )

  ans_obtained <- increments(object)
  expect_identical(ans_expected, ans_obtained)
})
