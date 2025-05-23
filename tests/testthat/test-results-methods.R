## 'as_tibble' ----------------------------------------------------------------

test_that("'as_tibble' works", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans_obtained <- tibble::as_tibble(x)
  ans_expected <- tibble::tibble(
    cohort = x$cohort,
    sex = x$sex,
    fitted = x$fitted
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'components' ---------------------------------------------------------------

test_that("'components' works when what is population", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- components(x, n_draw = 5)
  expect_setequal(ans$age, args$datamods[[1]]$data$age)
  expect_setequal(ans$sex, args$datamods[[1]]$data$sex)
  expect_setequal(ans$cohort, args$datamods[[1]]$data$cohort)
  expect_setequal(ans$time, args$datamods[[1]]$data$time)
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "population", "population.fitted",
      "population.lower", "population.upper"
    )
  )
})

test_that("'components' works when what is population - need to adjust popn", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  x$fitted[[1]]$mean[] <- c(1, 3, 4)
  ans <- components(x, n_draw = 500)
  expect_identical(names(ans), c("population", "propn_adjusted"))
})

test_that("'components' works when what is 'events'", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "events",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "births", "deaths", "ins", "outs",
      "ins.fitted", "ins.lower", "ins.upper",
      "outs.fitted", "outs.lower", "outs.upper"
    )
  )
})

test_that("'components' works when what is 'exposure'", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "exposure",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c("age", "sex", "cohort", "time", "exposure")
  )
})

test_that("'components' works when what is 'rates'", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "rates",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "births", "deaths", "ins", "outs",
      "births.fitted", "births.lower", "births.upper",
      "deaths.fitted", "deaths.lower", "deaths.upper",
      "ins.fitted", "ins.lower", "ins.upper",
      "outs.fitted", "outs.lower", "outs.upper"
    )
  )
})

test_that("'components' works when what is 'sysmods'", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "sysmods",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "births.mean", "births.disp",
      "deaths.mean", "deaths.disp",
      "ins.mean", "ins.disp",
      "outs.mean", "outs.disp"
    )
  )
})

test_that("'components' works when what is data_population, data_events", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = c("data_population", "data_events"),
    n_draw = 5
  )
  expect_true(is.list(ans))
  expect_identical(names(ans), c("data_population", "data_events"))
  expect_setequal(
    names(ans$data_population),
    c("age", "sex", "cohort", "time", "data_popn")
  )
  expect_setequal(
    names(ans$data_events),
    c("age", "sex", "cohort", "time", "data_births", "data_deaths", "data_ins", "data_outs")
  )
})

test_that("'components' works when what is everything", {
  set.seed(0)
  args <- sim_args_estimate(scale_sd = 0.1)
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = c(
      "population", "events", "exposure", "rates",
      "datamods", "sysmods", "data_population", "data_events"
    ),
    n_draw = 5
  )
  expect_true(is.list(ans))
  expect_setequal(names(ans), c(
    "population", "events", "exposure", "rates", "sysmods",
    "datamods", "data_population", "data_events"
  ))
})

test_that("'components' works collapse is 'cohort'", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = c(
      "population", "events", "exposure", "rates",
      "sysmods", "data_population", "data_events"
    ),
    n_draw = 5,
    collapse = "cohort"
  )
  expect_true(is.list(ans))
  expect_setequal(names(ans), c(
    "population", "events", "exposure", "rates", "sysmods",
    "data_population", "data_events"
  ))
  expect_true(all(sapply(ans, function(x) !("cohort" %in% names(x)))))
})



## 'diagnostics' --------------------------------------------------------------

test_that("'diagnostics' works", {
  set.seed(0)
  args <- sim_args_estimate()
  x <- do.call(estimate_account, args)
  ans <- diagnostics(x)
  expect_true(tibble::is_tibble(ans))
})
