## 'get_const_all' ------------------------------------------------------------

test_that("'get_const_all' works with valid inputs - non-empty", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_const_all(list(x, x)), c(x$ratio, x$sd, x$ratio, x$sd))
})

test_that("'get_const_all' works with valid inputs - empty", {
  expect_identical(get_const_all(list()), double())
})


## 'get_data_all' -------------------------------------------------------------

test_that("'get_data_all' works with valid inputs - non-empty", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_data_all(list(x, x)), c(x$data, x$data))
})

test_that("'get_data_all' works with valid inputs - empty", {
  expect_identical(get_data_all(list()), double())
})


## 'get_idx_const_all' --------------------------------------------------------

test_that("'get_idx_const_all' works with valid inputs'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_idx_const_all(list(x, x)), factor(rep(1:2, each = 22)))
})


## 'get_has_par_all' ----------------------------------------------------------

test_that("'get_has_par_all' works with valid inputs'", {
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
  expect_identical(get_has_par_all(list(x, x)), c(TRUE, TRUE))
})


## 'get_i_mod_all' ------------------------------------------------------------

test_that("'get_i_mod_all' works with valid inputs'", {
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
  expect_identical(get_i_mod_all(list(x, x)), c(101L, 101L))
})


## 'get_idx_data_all' ---------------------------------------------------------

test_that("'get_idx_data_all' works with valid inputs'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_idx_data_all(list(x, x)), factor(rep(1:2, each = 10)))
})


## 'get_idx_par_all' ---------------------------------------------------------

test_that("'get_idx_par_all' works with valid inputs'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_idx_par_all(list(x, x)), factor(1:2, levels = 1:2))
})


## 'get_is_obs_all' -----------------------------------------------------------

test_that("'get_is_obs_all' works with valid inputs - non-empty", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(
    get_is_obs_all(list(x, x)),
    c(0L, rep(1L, 9), 0L, rep(1L, 9))
  )
})

test_that("'get_is_obs_all' works with valid inputs - empty", {
  expect_identical(get_is_obs_all(list()), integer())
})


## 'get_par_all' -------------------------------------------------------------

test_that("'get_par_all' works with valid inputs - non-empty", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_par_all(list(x, x)), c(0, 0))
})

test_that("'get_par_all' works with valid inputs - empty", {
  expect_identical(get_par_all(list()), double())
})


## 'get_transform_datamod_all' -------------------------------------------------------------

test_that("'get_transform_datamod_all' works with valid inputs - non-empty", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_transform_datamod_all(list(x, x)), list(identity, identity))
})

test_that("'get_transform_datamod_all' works with valid inputs - empty", {
  expect_identical(get_transform_datamod_all(list()), list())
})
