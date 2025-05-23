## 'components_cohort' --------------------------------------------------------

test_that("'components_cohort' fails with unfitted cohort", {
  seed_list <- make_seed_account(seed_in = 1)
  x_non_fitted <- sim_comod(is_new_cohort = TRUE)
  expect_error(
    components_cohort(x_non_fitted, what = "population", n_draw = 10, seed_list = seed_list),
    "model not fitted"
  )
})

test_that("'components_cohort' works with new cohort, what = population", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = TRUE)
  x_fitted <- fit(x)
  ans <- components_cohort(x_fitted, what = "population", n_draw = 10, seed_list = seed_list)
  expect_identical(names(ans), c("population", "propn_adjusted"))
  expect_setequal(names(ans$population), c("age", "time", "population"))
})

test_that("'components_cohort' works with existing cohort, what = population", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = FALSE)
  x_fitted <- fit(x)
  ans <- components_cohort(x_fitted, what = "population", n_draw = 10, seed_list = seed_list)
  expect_identical(names(ans), c("population", "propn_adjusted"))
  expect_setequal(names(ans$population), c("age", "time", "population"))
})

test_that("'components_cohort' works with new cohort, what = events", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = TRUE)
  x_fitted <- fit(x)
  ans <- components_cohort(x_fitted, what = "events", n_draw = 10, seed_list = seed_list)
  expect_identical(names(ans), c("events", "propn_adjusted"))
  expect_identical(
    names(ans[["events"]]),
    c("time", "age", "births", "deaths", "ins", "outs")
  )
})

test_that("'components_cohort' works with existing cohort, what = events", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = FALSE)
  x_fitted <- fit(x)
  ans <- components_cohort(x_fitted, what = "events", n_draw = 10, seed_list = seed_list)
  expect_identical(names(ans), c("events", "propn_adjusted"))
  expect_identical(
    names(ans[["events"]]),
    c("time", "age", "births", "deaths", "ins", "outs")
  )
})

test_that("'components_cohort' works with existing cohort, what = sysmods", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = FALSE)
  x_fitted <- fit(x)
  ans <- components_cohort(x_fitted, what = "sysmods", n_draw = 10, seed_list = seed_list)
  expect_identical(names(ans), c("sysmods", "propn_adjusted"))
  expect_identical(
    names(ans[["sysmods"]]),
    c(
      "time", "age", "births.mean", "births.disp",
      "deaths.mean", "deaths.disp",
      "ins.mean", "ins.disp",
      "outs.mean", "outs.disp"
    )
  )
})

test_that("'components_cohort' works with new cohort, what = population, events", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = FALSE)
  x_fitted <- fit(x)
  ans <- components_cohort(x_fitted,
    what = c("population", "events"),
    n_draw = 10,
    seed_list = seed_list
  )
  expect_identical(names(ans), c("population", "events", "propn_adjusted"))
  times_popn <- unique(ans$population$time)
  times_events <- unique(ans$events$time)
  expect_identical(times_popn, c(times_events[1] - 1L, times_events))
})

test_that("'components_cohort' works with new cohort, what = everything", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(scale_sd = 0.3)
  x_fitted <- fit(x)
  ans <- components_cohort(x_fitted,
    what = c(
      "population", "events", "exposure",
      "rates", "sysmods", "datamods",
      "data_population", "data_events"
    ),
    n_draw = 10,
    seed_list = seed_list
  )
  expect_identical(names(ans), c(
    "population", "events", "exposure",
    "rates", "sysmods", "datamods",
    "data_population", "data_events", "propn_adjusted"
  ))
})


## 'components_cohort_account' ------------------------------------------------

test_that("'components_cohort_account' works - new cohort", {
  seed_list <- make_seed_account(seed_in = 1)
  comod <- sim_comod(is_new_cohort = TRUE)
  comod <- fit(comod)
  n_draw <- 10
  ans <- components_cohort_account(comod, n_draw = n_draw, seed_list = seed_list)
  expect_identical(names(ans), c("population", "events", "exposure", "propn_adjusted"))
  expect_equal(
    ans$population$population[[10]] - ans$population$population[[1]],
    Reduce("+", ans$events$ins[-c(1, 20)])
    - Reduce("+", ans$events$deaths[-c(1, 20)])
      - Reduce("+", ans$events$outs[-c(1, 20)])
  )
  expect_true(all(sapply(ans$births, length) == 2L))
})

test_that("'components_cohort_account' works - existing cohort", {
  seed_list <- make_seed_account(seed_in = 1)
  comod <- sim_comod()
  comod <- fit(comod)
  n_draw <- 10
  ans <- components_cohort_account(comod, n_draw = n_draw, seed_list = seed_list)
  expect_identical(names(ans), c("population", "events", "exposure", "propn_adjusted"))
  expect_equal(
    ans$population$population[[11L]] - ans$population$population[[1L]],
    Reduce("+", ans$events$ins)
    - Reduce("+", ans$events$deaths)
      - Reduce("+", ans$events$outs)
  )
  expect_true(all(sapply(ans$births, length) == 2L))
})


## 'components_cohort_data' ---------------------------------------------------

test_that("'components_cohort_data' works", {
  x <- sim_comod()
  x$datamods_stk <- c(x$datamods_stk, x$datamods_stk)
  names(x$datamods_stk) <- c("d1", "d2")
  x <- fit(x)
  n_draw <- 10
  ans <- components_cohort_data(x)
  expect_identical(names(ans), c("population", "events"))
  expect_identical(names(ans$population), c("time", "age", "d1", "d2"))
  expect_identical(
    names(ans$events),
    c("time", "age", "dataset_bth", "dataset_dth", "dataset_ins", "dataset_outs")
  )
})


## 'components_cohort_datamods' -----------------------------------------------

test_that("'components_cohort_datamods' works - has parameters", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(scale_sd = 0.1)
  x <- fit(x)
  n_draw <- 10
  comp_acc <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  ans <- components_cohort_datamods(
    object = x,
    population = comp_acc$population,
    events = comp_acc$events,
    seed_list = seed_list
  )
  expect_true(nrow(ans) > 0L)
  expect_identical(names(ans), c("series", "data", "par", "value"))
})

test_that("'components_cohort_datamods' works - no parameters", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp_acc <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  ans <- components_cohort_datamods(
    object = x,
    population = comp_acc$population,
    events = comp_acc$events,
    seed_list = seed_list
  )
  expect_identical(nrow(ans), 0L)
  expect_identical(names(ans), c("series", "data", "par", "value"))
})


## 'components_cohort_rates' --------------------------------------------------

test_that("'components_cohort_rates' works - no births", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = TRUE)
  x <- fit(x)
  n_draw <- 10
  account <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  ans <- components_cohort_rates(
    object = x,
    account = account,
    n_draw = n_draw,
    seed_list = seed_list
  )
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c("time", "age", "births", "deaths", "ins", "outs"))
  expect_true(all(is.na(unlist(ans$births))))
})

test_that("'components_cohort_rates' works - has births", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(is_new_cohort = FALSE)
  x <- fit(x)
  n_draw <- 10
  account <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  ans <- components_cohort_rates(
    object = x,
    account = account,
    n_draw = n_draw,
    seed_list = seed_list
  )
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c("time", "age", "births", "deaths", "ins", "outs"))
  expect_false(anyNA(unlist(ans$births)))
})


## 'components_cohort_sysmods' ------------------------------------------------

test_that("'components_cohort_sysmods' works", {
  x <- sim_comod(is_new_cohort = TRUE)
  x <- fit(x)
  n_draw <- 5
  ans <- components_cohort_sysmods(x)
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c(
    "time", "age", "births.mean", "births.disp",
    "deaths.mean", "deaths.disp",
    "ins.mean", "ins.disp", "outs.mean", "outs.disp"
  ))
})


## 'increments_codatamod_all' -------------------------------------------------

test_that("'increments_codatamod_all' works", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  datamods <- list(df1 = x, df2 = x)
  ans_obtained <- increments_codatamod_all(datamods, cohort = 2000L, is_stock = TRUE)
  ans_expected <- tibble::tibble(
    time = 2001:2004,
    df1 = rep(2, 4),
    df2 = rep(2, 4)
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'increments_comod' ---------------------------------------------------------

test_that("'increments_comod' works", {
  x <- sim_comod(is_new_cohort = TRUE)
  ans <- increments_comod(x)
  expect_identical(
    names(ans),
    c(
      "time",
      names(x$datamods_stk),
      x$count_bthdth$nm_data_dth[[1]],
      names(x$datamods_ins),
      names(x$datamods_outs)
    )
  )
})


## 'increments_deaths' --------------------------------------------------------

test_that("'increments_deaths' works - is new cohort, is extinct cohort", {
  count_bthdth <- data.frame(
    val_dth = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    nm_data_dth = rep("df1", 10)
  )
  ans_obtained <- increments_deaths(
    count_bthdth = count_bthdth,
    cohort = 2000L
  )
  ans_expected <- tibble::tibble(
    time = 2001:2004,
    df1 = c(5, 9, 13, 17)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'increments_codatamod' - is not new cohort, is not extinct cohort", {
  count_bthdth <- data.frame(
    val_dth = as.double(1:10),
    age = c(10L, rep(11:14, each = 2), 15L),
    time = rep(2001:2005, each = 2),
    nm_data_dth = rep("df1", 10)
  )
  ans_obtained <- increments_deaths(
    count_bthdth = count_bthdth,
    cohort = 1990L
  )
  ans_expected <- tibble::tibble(
    time = 2001:2005,
    df1 = c(3, 7, 11, 15, 19)
  )
  expect_identical(ans_obtained, ans_expected)
})
