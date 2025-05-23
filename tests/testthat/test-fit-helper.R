## 'is_positive_definite' -----------------------------------------------------

test_that("'is_positive_definite' works with valid inputs", {
  set.seed(0)
  n <- 10
  ans <- logical(n)
  for (i in 1:n) {
    a <- matrix(rnorm(20), nr = 4)
    m <- crossprod(a)
    ans[i] <- is_positive_definite(m)
  }
  expect_true(all(ans))
})

## 'make_fail_mean' -----------------------------------------------------------

test_that("'make_fail_mean' works with valid inputs", {
  parameters <- list(
    log_stk_init = -1,
    log_val_ins = c(0.1, 0.2),
    log_val_outs = c(0.2, 0.3)
  )
  ans_obtained <- make_fail_mean(parameters)
  ans_expected <- c(
    log_stk_init = NA_real_,
    log_val_ins = NA_real_,
    log_val_ins = NA_real_,
    log_val_outs = NA_real_,
    log_val_outs = NA_real_
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'make_fail_var' ------------------------------------------------------------

test_that("'make_fail_var' works with valid inputs", {
  parameters <- list(
    log_stk_init = -1,
    log_val_ins = c(0.1, 0.2),
    log_val_outs = c(0.2, 0.3)
  )
  ans_obtained <- make_fail_var(parameters)
  nms <- c(
    "log_stk_init",
    "log_val_ins",
    "log_val_ins",
    "log_val_outs",
    "log_val_outs"
  )
  ans_expected <- matrix(0, nr = 5, nc = 5, dimnames = list(nms, nms))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_map' -----------------------------------------------------------------

test_that("'make_map' works with valid inputs - map is NULL", {
  prior_stk_init <- list(mean = 3, sd = 1)
  expect_identical(make_map(prior_stk_init), NULL)
})

test_that("'make_map' works with valid inputs - map is list", {
  prior_stk_init <- list(mean = 3, sd = 0)
  expect_identical(
    make_map(prior_stk_init),
    list(log_val_stk_init = factor(NA))
  )
})


## 'make_parameters' ----------------------------------------------------------

test_that("'make_parameters' works with valid inputs", {
  x <- sim_comod()
  ans <- make_parameters(
    mean_stk_init = x$prior_stk_init$mean,
    sd_stk_init = x$prior_stk_init$sd,
    val_dth <- x$count_bthdth$val_dth,
    mean_ins = x$sysmod_ins$mean,
    mean_outs = x$sysmod_outs$mean,
    datamods_stk = x$datamods_stk,
    datamods_ins = x$datamods_ins,
    datamods_outs = x$datamods_outs
  )
  expect_true(exp(ans$log_val_stk_init) > 0.001)
  expect_true(all(ans$log_vals_ins >= 0.001))
  expect_true(all(ans$log_vals_outs >= 0.001))
  expect_true(all(exp(ans$log_val_stk_init)
  + cumsum(exp(ans$log_val_ins))
    - cumsum(exp(ans$log_val_outs))
    - cumsum(val_dth) >= 0.001))
})


## 'make_vals_init' -----------------------------------------------------------

test_that("'make_vals_init' works with valid inputs - stk_init fixed", {
  K <- 10
  sd_stk_init <- 0
  for (seed in 1:10) {
    set.seed(seed)
    mean_stk_init <- runif(1, max = 10)
    val_dth <- rpois(K, lambda = 2)
    mean_ins <- runif(K, max = 5)
    mean_outs <- runif(K, max = 1)
    ans <- make_vals_init(
      mean_stk_init = mean_stk_init,
      sd_stk_init = sd_stk_init,
      val_dth = val_dth,
      mean_ins = mean_ins,
      mean_outs = mean_outs
    )
    expect_true(all(exp(ans$log_val_stk_init)
    + cumsum(exp(ans$log_val_ins))
      - cumsum(exp(ans$log_val_outs))
      - cumsum(val_dth) >= 0.000001))
    expect_true(exp(ans$log_val_stk_init) >= 0.000001)
    expect_true(all(exp(ans$log_val_ins) >= 0.000001))
    expect_true(all(exp(ans$log_val_outs) >= 0.000001))
  }
})

test_that("'make_vals_init' works with valid inputs - stk_init varying", {
  K <- 10
  for (seed in 1:10) {
    set.seed(seed)
    mean_stk_init <- runif(1, max = 10)
    sd_stk_init <- 2
    mean_ins <- runif(K, max = 5)
    mean_outs <- runif(K, max = 1)
    val_dth <- rpois(K, lambda = 2)
    ans <- make_vals_init(
      mean_stk_init = mean_stk_init,
      sd_stk_init = sd_stk_init,
      val_dth = val_dth,
      mean_ins = mean_ins,
      mean_outs = mean_outs
    )
    expect_true(all(exp(ans$log_val_stk_init)
    + cumsum(exp(ans$log_val_ins))
      - cumsum(exp(ans$log_val_outs))
      - cumsum(val_dth) >= 0.000001))
    expect_true(exp(ans$log_val_stk_init) >= 0.000001)
    expect_true(all(exp(ans$log_val_ins) >= 0.000001))
    expect_true(all(exp(ans$log_val_outs) >= 0.000001))
  }
})


test_that("'make_vals_init' works with valid inputs - stk_init fixed and zero", {
  K <- 10
  sd_stk_init <- 0
  for (seed in 1:10) {
    set.seed(seed)
    mean_stk_init <- 0
    val_dth <- rpois(K, lambda = 2)
    mean_ins <- runif(K, max = 5)
    mean_outs <- runif(K, max = 1)
    ans <- make_vals_init(
      mean_stk_init = mean_stk_init,
      sd_stk_init = sd_stk_init,
      val_dth = val_dth,
      mean_ins = mean_ins,
      mean_outs = mean_outs
    )
    expect_true(all(exp(ans$log_val_stk_init)
    + cumsum(exp(ans$log_val_ins))
      - cumsum(exp(ans$log_val_outs))
      - cumsum(val_dth) >= 0.000001))
    expect_true(exp(ans$log_val_stk_init) >= 0.000001)
    expect_true(all(exp(ans$log_val_ins) >= 0.000001))
    expect_true(all(exp(ans$log_val_outs) >= 0.000001))
  }
})
