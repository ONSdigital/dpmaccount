## 'new_codatamod_norm' --------------------------------------------------

test_that("'new_codatamod_norm' works with valid inputs - scale_ratio, scale_sd both 0", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = 0,
    scale_sd = 0
  )
  ans <- new_codatamod_norm(args)
  expect_identical(ans$i_mod, 1L)
  expect_s3_class(ans, "dpmaccount_codatamod_norm_nopar")
  expect_s3_class(ans, "dpmaccount_codatamod_norm")
  expect_s3_class(ans, "dpmaccount_codatamod")
})

test_that("'new_codatamod_norm' works with valid inputs - scale_ratio is non-zero", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = 0.1,
    scale_sd = 0
  )
  ans <- new_codatamod_norm(args)
  expect_identical(ans$i_mod, 101L)
  expect_identical(ans$scale_ratio, 0.1)
  expect_s3_class(ans, "dpmaccount_codatamod_norm_haspar")
  expect_s3_class(ans, "dpmaccount_codatamod_norm")
  expect_s3_class(ans, "dpmaccount_codatamod")
})

test_that("'new_codatamod_norm' works with valid inputs - scale_sd is non-zero", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = 0,
    scale_sd = 0.1
  )
  ans <- new_codatamod_norm(args)
  expect_identical(ans$i_mod, 101L)
  expect_identical(ans$scale_sd, 0.1)
  expect_s3_class(ans, "dpmaccount_codatamod_norm_haspar")
  expect_s3_class(ans, "dpmaccount_codatamod_norm")
  expect_s3_class(ans, "dpmaccount_codatamod")
})


## 'new_codatamod_t' ----------------------------------------------------------

test_that("'new_codatamod_norm' works with valid inputs", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10)
  )
  ans <- new_codatamod_t(args)
  expect_s3_class(ans, "dpmaccount_codatamod_t")
  expect_s3_class(ans, "dpmaccount_codatamod")
})


## 'validate_codatamod' --------------------------------------------------

test_that("'validate_codatamod' works with valid inputs", {
  args <- data.frame(
    data = c(NA, as.double(2:10)),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_sd = rep(1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(validate_codatamod(x), x)
})

test_that("'validate_codatamod' throws expected error when 'data' and 'is_obs' inconsistent", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(rep(1L, 9), 0L),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_sd = rep(1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_error(
    validate_codatamod(x),
    "element 10 of 'data' is 10 but element 10 of 'is_obs' is 0"
  )
})


## 'validate_codatamod_norm' ---------------------------------------------

test_that("'validate_codatamod_norm' works with valid inputs", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0, 10)
  )
  x <- new_codatamod_norm(args)
  expect_identical(validate_codatamod_norm(x), x)
})

test_that("'validate_codatamod_norm' throws expected error when 'sd' is NA", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    ratio = rep(1, 10),
    sd = c(rep(0.25, 9), NA),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x <- new_codatamod_norm(args)
  expect_error(
    validate_codatamod_norm(x),
    "element 10 of 'sd' is NA \\(and element 10 of 'data' is not\\)"
  )
})


## 'validate_codatamod_t' ---------------------------------------------

test_that("'validate_codatamod_t' works with valid inputs", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = rep(0.25, 10)
  )
  x <- new_codatamod_t(args)
  expect_identical(validate_codatamod_t(x), x)
})

test_that("'validate_codatamod_t' throws expected error when 'scale' is NA", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = rep(1L, 10),
    df = rep(4, 10),
    ratio = rep(1, 10),
    scale = c(rep(0.25, 9), NA)
  )
  x <- new_codatamod_t(args)
  expect_error(
    validate_codatamod_t(x),
    "element 10 of 'scale' is NA \\(and element 10 of 'data' is not\\)"
  )
})
