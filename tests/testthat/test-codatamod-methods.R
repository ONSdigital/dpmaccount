## 'get_const' ----------------------------------------------------------------

test_that("'get_const' works with 'dpmaccount_codatamod_norm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0.2, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_const(x), c(x$ratio, x$sd, x$scale_ratio, x$scale_sd))
})

test_that("'get_const' works with 'dpmaccount_codatamod_norm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = 0,
    scale_sd = 0,
    sd = rep(0.25, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_const(x), c(x$ratio, x$sd))
})

test_that("'get_const' works with 'dpmaccount_codatamod_t_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_const(x), c(x$ratio, x$scale, x$df[1], x$scale_ratio))
})

test_that("'get_const' works with 'dpmaccount_codatamod_t_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_const(x), c(x$ratio, x$scale, x$df[1]))
})

test_that("'get_const' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_const(x), c(x$ratio, x$disp, x$scale_ratio))
})

test_that("'get_const' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_const(x), c(x$ratio, x$disp))
})

test_that("'get_const' works with 'dpmaccount_codatamod_poisson_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_const(x), c(x$ratio, x$scale_ratio))
})

test_that("'get_const' works with 'dpmaccount_codatamod_poisson_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_const(x), c(x$ratio))
})

test_that("'get_const' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_const(x), c(x$ratio, x$sd, x$scale_ratio))
})

test_that("'get_const' works with 'dpmaccount_codatamod_lognorm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = 0,
    sd = rep(0.25, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_const(x), c(x$ratio, x$sd))
})

## 'get_data' -----------------------------------------------------------------

test_that("'get_data' works with 'dpmaccount_codatamod_norm'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_data(x), x$data)
})

test_that("'get_data' works with 'dpmaccount_codatamod_t'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_data(x), x$data)
})

test_that("'get_data' works with 'dpmaccount_codatamod_nbinom'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_data(x), x$data)
})

test_that("'get_data' works with 'dpmaccount_codatamod_poisson'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_data(x), x$data)
})

test_that("'get_data' works with 'dpmaccount_codatamod_lognorm'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_data(x), x$data)
})

## 'get_data_cols' ------------------------------------------------------------

test_that("'get_data_cols' works with 'dpmaccount_codatamod_norm'", {
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
  ans_obtained <- get_data_cols(x, nm = "dataset1")
  ans_expected <- tibble::tibble(
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    age = rep(0:4, each = 2),
    dataset1 = as.double(1:10)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_data_cols' works with 'dpmaccount_codatamod_t'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10),
    scale = rep(0.25, 10)
  )
  x <- new_codatamod_t(args)
  ans_obtained <- get_data_cols(x, nm = "dataset1")
  ans_expected <- tibble::tibble(
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    age = rep(0:4, each = 2),
    dataset1 = as.double(1:10)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_data_cols' works with 'dpmaccount_codatamod_nbinom'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  ans_obtained <- get_data_cols(x, nm = "dataset1")
  ans_expected <- tibble::tibble(
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    age = rep(0:4, each = 2),
    dataset1 = as.double(1:10)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_data_cols' works with 'dpmaccount_codatamod_poisson'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.1, 10)
  )
  x <- new_codatamod_poisson(args)
  ans_obtained <- get_data_cols(x, nm = "dataset1")
  ans_expected <- tibble::tibble(
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    age = rep(0:4, each = 2),
    dataset1 = as.double(1:10)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_data_cols' works with 'dpmaccount_codatamod_lognorm'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  ans_obtained <- get_data_cols(x, nm = "dataset1")
  ans_expected <- tibble::tibble(
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    age = rep(0:4, each = 2),
    dataset1 = as.double(1:10)
  )
  expect_identical(ans_obtained, ans_expected)
})

## 'get_has_par' -----------------------------------------------------------------

test_that("'get_has_par' works with 'dpmaccount_codatamod_norm_haspar'", {
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
  expect_true(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_norm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = 0
  )
  x <- new_codatamod_norm(args)
  expect_false(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_t_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(2, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  expect_true(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_t_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(2, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_false(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_true(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_false(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_poisson_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.3, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_true(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_poisson_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_false(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0.3, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_true(get_has_par(x))
})

test_that("'get_has_par' works with 'dpmaccount_codatamod_lognorm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_false(get_has_par(x))
})


## 'get_i_mod' -----------------------------------------------------------------

test_that("'get_i_mod' works with 'dpmaccount_codatamod_norm_haspar'", {
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
  expect_identical(get_i_mod(x), 101L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_norm_nopar'", {
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
  expect_identical(get_i_mod(x), 1L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_t_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_i_mod(x), 201L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_t_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_i_mod(x), 2L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_i_mod(x), 301L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_i_mod(x), 3L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_poisson_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.1, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_i_mod(x), 401L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_poisson_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_i_mod(x), 4L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0.3, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_i_mod(x), 501L)
})

test_that("'get_i_mod' works with 'dpmaccount_codatamod_lognorm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_i_mod(x), 5L)
})

## 'get_is_obs' ---------------------------------------------------------------

test_that("'get_is_obs' works with 'dpmaccount_codatamod_norm'", {
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
  expect_identical(get_is_obs(x), c(0L, rep(1L, 9)))
})


## 'get_nms_par' -----------------------------------------------------------------

test_that("'get_nms_par' works with 'dpmaccount_codatamod_norm_haspar' - 1 par", {
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
  expect_identical(get_nms_par(x), "mult_sd")
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_norm_haspar' - 2 pars", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(1, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_nms_par(x), c("mult_ratio", "mult_sd"))
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_norm_nopar'", {
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
  expect_identical(get_nms_par(x), character())
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_t_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_nms_par(x), "mult_ratio")
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_t_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_nms_par(x), character())
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_nms_par(x), "mult_ratio")
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_nms_par(x), character())
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_poisson_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_nms_par(x), "mult_ratio")
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_poisson_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_nms_par(x), character())
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0.3, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_nms_par(x), "mult_ratio")
})

test_that("'get_nms_par' works with 'dpmaccount_codatamod_lognorm_nopar'", {
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
  x <- new_codatamod_lognorm(args)
  expect_identical(get_nms_par(x), character())
})

## 'get_par' -----------------------------------------------------------------

test_that("'get_par' works with 'dpmaccount_codatamod_norm_haspar' - one parameter", {
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
  expect_identical(get_par(x), 0)
})

test_that("'get_par' works with 'dpmaccount_codatamod_norm_haspar' - two parameters", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(1, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_par(x), c(0, 0))
})

test_that("'get_par' works with 'dpmaccount_codatamod_norm_nopar'", {
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
  expect_identical(get_par(x), double())
})

test_that("'get_par' works with 'dpmaccount_codatamod_t_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_par(x), 0)
})

test_that("'get_par' works with 'dpmaccount_codatamod_t_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_par(x), double())
})


test_that("'get_par' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_par(x), 0)
})

test_that("'get_par' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_par(x), double())
})

test_that("'get_par' works with 'dpmaccount_codatamod_poisson_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_par(x), 0)
})

test_that("'get_par' works with 'dpmaccount_codatamod_poisson_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_par(x), double())
})

test_that("'get_par' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_par(x), 0)
})

test_that("'get_par' works with 'dpmaccount_codatamod_lognorm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_par(x), double())
})

## 'get_transform_datamod' ----------------------------------------------------

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_norm_haspar' - 1 par", {
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
  expect_identical(get_transform_datamod(x), list(identity))
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_norm_haspar' - 2 par", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(1, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(get_transform_datamod(x), list(identity, identity))
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_norm_nopar'", {
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
  expect_identical(get_transform_datamod(x), list())
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_t_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_transform_datamod(x), list(identity))
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_t_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(get_transform_datamod(x), list())
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_transform_datamod(x), list(identity))
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(get_transform_datamod(x), list())
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_poisson_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.1, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_transform_datamod(x), list(identity))
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_poisson_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(get_transform_datamod(x), list())
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.1, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_transform_datamod(x), list(identity))
})

test_that("'get_transform_datamod' works with 'dpmaccount_codatamod_lognorm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(get_transform_datamod(x), list())
})

## 'increments_codatamod' -----------------------------------------------------

test_that("'increments_codatamod' works with 'dpmaccount_codatamod_norm_haspar' - not stock, is new cohort, is extinct cohort", {
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
  ans_obtained <- increments_codatamod(x, cohort = 2000L, nm_data = "df1", is_stock = FALSE)
  ans_expected <- tibble::tibble(
    time = 2001:2004,
    df1 = c(5, 9, 13, 17)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'increments_codatamod' works with 'dpmaccount_codatamod_norm_haspar' - not stock, is not new cohort, is not extinct cohort", {
  args <- data.frame(
    data = as.double(1:10),
    age = c(10L, rep(11:14, each = 2), 15L),
    time = rep(2001:2005, each = 2),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  ans_obtained <- increments_codatamod(x, cohort = 1990L, nm_data = "df1", is_stock = FALSE)
  ans_expected <- tibble::tibble(
    time = 2001:2005,
    df1 = c(3, 7, 11, 15, 19)
  )
  expect_identical(ans_obtained, ans_expected)
})


test_that("'increments_codatamod' works with 'dpmaccount_codatamod_norm_haspar' - is stock, is not new cohort, is_extinct cohort", {
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
  ans_obtained <- increments_codatamod(x, cohort = 2000L, nm_data = "df1", is_stock = TRUE)
  ans_expected <- tibble::tibble(
    time = 2001:2004,
    df1 = rep(2, 4)
  )
  expect_identical(ans_obtained, ans_expected)
})

test_that("'increments_codatamod' works with 'dpmaccount_codatamod_norm_haspar' - is stock, is not new cohort, is not extinct cohort", {
  args <- data.frame(
    data = as.double(1:9),
    age = c(rep(11:14, each = 2), 15L),
    time = c(2000L, rep(2001:2004, each = 2)),
    is_obs = c(0L, rep(1L, 8)),
    ratio = rep(1, 9),
    sd = rep(0.25, 9),
    scale_ratio = rep(0, 9),
    scale_sd = rep(0.1, 9)
  )
  x <- new_codatamod_norm(args)
  ans_obtained <- increments_codatamod(x, cohort = 1989L, nm_data = "df1", is_stock = TRUE)
  ans_expected <- tibble::tibble(
    time = 2001:2004,
    df1 = rep(2, 4)
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'make_str' -----------------------------------------------------------------

test_that("'make_str' works with 'dpmaccount_codatamod_norm_haspar - scale_ratio = 0, scale_sd > 0'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ N(ratio * ins, (exp(beta) * sd)^2)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_norm_haspar - scale_ratio > 0, scale_sd = 0'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(1, 10),
    scale_sd = rep(0, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ N(exp(alpha) * ratio * ins, sd^2)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_norm_haspar - scale_ratio > 0, scale_sd > 0'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(1, 10),
    scale_sd = rep(1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ N(exp(alpha) * ratio * ins, (exp(beta) * sd)^2)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_norm_nopar'", {
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
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ N(ratio * ins, sd^2)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_t_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ t(df, exp(alpha) * ratio * ins, scale^2)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_t_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ t(df, ratio * ins, scale^2)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ nbinom(exp(alpha) * ratio * ins, disp)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ nbinom(ratio * ins, disp)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_poisson_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.1, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ poisson(exp(alpha) * ratio * ins)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_poisson_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ poisson(ratio * ins)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.1, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ LN(exp(alpha) * ratio * ins, sd^2)\n"
  )
})

test_that("'make_str' works with 'dpmaccount_codatamod_lognorm_nopar'", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  expect_identical(
    make_str(x, nm_data = "indata", nm_series = "ins"),
    "      indata ~ LN(ratio * ins, sd^2)\n"
  )
})


## 'rgen' -----------------------------------------------------------------

test_that("'rgen' works with 'dpmaccount_codatamod_norm_haspar' - one parameter", {
  set.seed(1)
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
  gened_data <- rgen(x)
  set.seed(1)
  alpha_c <- rnorm(length(args$data), mean = 0, sd = args$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)
  beta_c <- rnorm(length(args$data), mean = 0, sd = args$scale_sd) #  β_c \sim \mathcal{N}(0, A_{β}^2)
  check_data <- rnorm(length(args$data), mean = exp(alpha_c) * args$ratio * args$data, sd = exp(beta_c) * args$sd)
  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_norm_haspar' - two parameters", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(1, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  gened_data <- rgen(x)
  set.seed(1)
  alpha_c <- rnorm(length(args$data), mean = 0, sd = args$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)
  beta_c <- rnorm(length(args$data), mean = 0, sd = args$scale_sd) #  β_c \sim \mathcal{N}(0, A_{β}^2)
  check_data <- rnorm(length(args$data), mean = exp(alpha_c) * args$ratio * args$data, sd = exp(beta_c) * args$sd)
  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_norm_nopar'", {
  set.seed(1)
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
  gened_data <- rgen(x)
  set.seed(1)
  alpha_c <- rnorm(length(args$data), mean = 0, sd = args$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)
  beta_c <- rnorm(length(args$data), mean = 0, sd = args$scale_sd) #  β_c \sim \mathcal{N}(0, A_{β}^2)
  check_data <- rnorm(length(args$data), mean = exp(alpha_c) * args$ratio * args$data, sd = exp(beta_c) * args$sd)
  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_t_haspar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_t(args)
  gened_data <- rgen(x)
  set.seed(1)
  alpha_c <- rnorm(length(args$data), mean = 0, sd = args$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)

  check_data <- rt(length(x$data), df = x$df) * x$scale + x$data * x$ratio * exp(alpha_c)
  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_t_nopar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_t(args)
  gened_data <- rgen(x)
  set.seed(1)
  alpha_c <- rnorm(length(args$data), mean = 0, sd = args$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)

  check_data <- rt(length(x$data), df = x$df) * x$scale + x$data * x$ratio * exp(alpha_c)
  expect_equal(gened_data$data, check_data)
})


test_that("'rgen' works with 'dpmaccount_codatamod_nbinom_haspar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_nbinom(args)
  gened_data <- rgen(x)
  set.seed(1)

  check_data <- rnbinom(length(x$data), mu = x$ratio * x$data, size = 1 / x$disp)

  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_nbinom_nopar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    disp = rep(0.1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_nbinom(args)
  gened_data <- rgen(x)
  set.seed(1)

  check_data <- rnbinom(length(x$data), mu = x$ratio * x$data, size = 1 / x$disp)

  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_poisson_haspar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0.2, 10)
  )
  x <- new_codatamod_poisson(args)
  gened_data <- rgen(x)
  set.seed(1)

  check_data <- rpois(length(x$data), lambda = x$ratio * x$data)

  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_poisson_nopar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_poisson(args)
  gened_data <- rgen(x)
  set.seed(1)

  check_data <- rpois(length(x$data), lambda = x$ratio * x$data)

  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_lognorm_haspar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0.3, 10)
  )
  x <- new_codatamod_lognorm(args)
  gened_data <- rgen(x)
  set.seed(1)
  alpha_c <- stats::rnorm(length(args$data), mean = 0, sd = args$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)
  check_data <- stats::rlnorm(length(args$data), mean = log((exp(alpha_c) * args$ratio * args$data)^2 / sqrt(args$sd^2 + (exp(alpha_c) * args$ratio * args$data)^2)), sd = sqrt(log(1 + (args$sd^2 / (exp(alpha_c) * args$ratio * args$data)^2))))
  expect_equal(gened_data$data, check_data)
})

test_that("'rgen' works with 'dpmaccount_codatamod_lognorm_nopar'", {
  set.seed(1)
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10)
  )
  x <- new_codatamod_lognorm(args)
  gened_data <- rgen(x)
  set.seed(1)
  alpha_c <- stats::rnorm(length(args$data), mean = 0, sd = args$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)
  check_data <- stats::rlnorm(length(args$data), mean = log((exp(alpha_c) * args$ratio * args$data)^2 / sqrt(args$sd^2 + (exp(alpha_c) * args$ratio * args$data)^2)), sd = sqrt(log(1 + (args$sd^2 / (exp(alpha_c) * args$ratio * args$data)^2))))
  expect_equal(gened_data$data, check_data)
})
