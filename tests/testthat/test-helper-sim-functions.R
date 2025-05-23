## 'project_cohort' -----------------------------------------------------------

test_that("'project_cohort' works with new cohorts", {
  set.seed(0)
  prior_stk_init <- list(
    mean = 100,
    sd = 0
  )
  sysmod_bth <- list(
    mean = rep(0, 20),
    disp = rep(0, 20)
  )
  sysmod_dth <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  sysmod_ins <- list(
    mean = rep(4, 20),
    disp = rep(0.1, 20)
  )
  sysmod_outs <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  ans <- project_cohort(
    is_new_cohort = TRUE,
    prior_stk_init = prior_stk_init,
    sysmod_bth = sysmod_bth,
    sysmod_dth = sysmod_dth,
    sysmod_ins = sysmod_ins,
    sysmod_outs = sysmod_outs
  )
  expect_true(identical(length(ans$val_stk), 21L))
  expect_true(identical(length(ans$val_bth), 20L))
  expect_true(all(unlist(ans[c("val_dth", "val_ins", "val_outs")]) >= 0))
  expect_true(any(unlist(ans[c("val_dth", "val_ins", "val_outs")]) > 0))
  expect_identical(
    diff(ans$val_stk),
    ans$val_ins - ans$val_dth - ans$val_outs
  )
})

test_that("'project_cohort' works with existing cohorts", {
  set.seed(0)
  prior_stk_init <- list(
    mean = 100,
    sd = Inf
  )
  sysmod_bth <- list(
    mean = rep(0, 20),
    disp = rep(0, 20)
  )
  sysmod_dth <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  sysmod_ins <- list(
    mean = rep(4, 20),
    disp = rep(0.1, 20)
  )
  sysmod_outs <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  ans <- project_cohort(
    is_new_cohort = FALSE,
    prior_stk_init = prior_stk_init,
    sysmod_bth = sysmod_bth,
    sysmod_dth = sysmod_dth,
    sysmod_ins = sysmod_ins,
    sysmod_outs = sysmod_outs
  )
  expect_true(identical(length(ans$val_stk), 21L))
  expect_true(identical(length(ans$val_bth), 20L))
  expect_true(all(unlist(ans) >= 0))
  expect_true(any(unlist(ans) > 0))
  expect_identical(
    diff(ans$val_stk),
    ans$val_ins - ans$val_dth - ans$val_outs
  )
})


## 'sim_arg_system_models' and 'sim_arg_data_models'----------------------------
test_that("sim_arg_system_models throws error when 'n_age' is less than 2", {
  expect_error(
    sim_arg_system_models(
      n_age = 1L,
      n_time = 3L,
      base_year = 2010L,
      disp_rates = 0.1
    ),
    "'n_age' is less than 2"
  )
})

test_that("sim_arg_data_models throws error when 'n_age' is less than 2", {
  expect_error(
    sim_arg_data_models(
      n_age = 1L,
      n_time = 3L,
      base_year = 2010L,
      scale_sd = 0,
      datamod_popn = TRUE,
      datamod_ins = TRUE,
      datamod_outs = TRUE
    ),
    "'n_age' is less than 2"
  )
})

test_that("'sim_arg_system_models' and 'sim_arg_data_models' works", {
  ans <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  expect_true(all(sapply(ans$sysmods, inherits, "dpmaccount_sysmod")))
  expect_true(all(sapply(ans$datamods, inherits, "dpmaccount_datamod")))
})

## 'sim_classif_vars_events' --------------------------------------------------

test_that("'sim_classif_vars_events' works with no cohort", {
  expect_identical(
    sim_classif_vars_events(n_age = 2, n_time = 2, has_cohort = FALSE),
    expand.grid(
      age = 0:1, sex = c("Female", "Male"), time = 2010:2011,
      KEEP.OUT.ATTRS = FALSE
    )
  )
})

test_that("'sim_classif_vars_events' works with cohort", {
  ans <- sim_classif_vars_events(n_age = 2, n_time = 2, has_cohort = TRUE)
  expect_identical(
    unique(ans[c("age", "sex", "time")]),
    expand.grid(
      age = 0:1, sex = c("Female", "Male"), time = 2010:2011,
      KEEP.OUT.ATTRS = FALSE
    )
  )
  ans_obtained <- ans[ans$cohort == 2010 & ans$sex == "Female", c("age", "time")]
  rownames(ans_obtained) <- NULL
  ans_expected <- data.frame(age = c(0L, 1L, 0L), time = c(2010L, 2011L, 2011L))
  expect_identical(ans_obtained, ans_expected)
})


## 'sim_classif_vars_popn' ----------------------------------------------------

test_that("'sim_classif_vars_popn' works", {
  ans_obtained <-
    expect_identical(
      sim_classif_vars_popn(n_age = 2, n_time = 2),
      expand.grid(
        age = 0:1, sex = c("Female", "Male"), time = 2009:2011,
        KEEP.OUT.ATTRS = FALSE
      )
    )
})


## 'sim_codatamod' ------------------------------------------------------------

test_that("'sim_codatamod' works with valid data", {
  x <- sim_codatamod(
    val = rep(c(3, NA), times = 5),
    sd_relative = 0.05,
    scale_ratio = 0.1,
    scale_sd = 0.1,
    is_obs = rep(c(1L, 0L), times = 5),
    time = rep(2001:2005, each = 2),
    age = c(10L, rep(11:14, each = 2), 15L)
  )
  expect_s3_class(x, "dpmaccount_codatamod_norm")
  expect_identical(validate_codatamod(x), x)
})


## 'sim_comod' ----------------------------------------------------------------

test_that("'sim_comod' works with new cohort", {
  x <- sim_comod(is_new_cohort = TRUE)
  expect_s3_class(x, "dpmaccount_comod")
})

test_that("'sim_comod' works with existing cohort", {
  x <- sim_comod(is_new_cohort = FALSE)
  expect_s3_class(x, "dpmaccount_comod")
})


## 'sim_time_age' -------------------------------------------------------------

test_that("'sim_time_age' works with new cohorts", {
  expect_identical(
    sim_time_age(K = 1L, cohort = 2015L, is_new_cohort = TRUE),
    data.frame(time = 2015L, age = 0L)
  )
  expect_identical(
    sim_time_age(K = 2L, cohort = 2015L, is_new_cohort = TRUE),
    data.frame(time = 2015:2016, age = c(0L, 0L))
  )
  expect_identical(
    sim_time_age(K = 4L, cohort = 2015L, is_new_cohort = TRUE),
    data.frame(
      time = c(2015:2016, 2016:2017),
      age = c(0L, 0L, 1L, 1L)
    )
  )
})

test_that("'sim_time_age' works with existing cohorts", {
  expect_identical(
    sim_time_age(K = 1L, cohort = 1990L, is_new_cohort = FALSE),
    data.frame(time = 2021L, age = 30L)
  )
  expect_identical(
    sim_time_age(K = 2L, cohort = 1990L, is_new_cohort = FALSE),
    data.frame(time = c(2021L, 2021L), age = 30:31)
  )
  expect_identical(
    sim_time_age(K = 4L, cohort = 1990L, is_new_cohort = FALSE),
    data.frame(
      time = c(2021L, 2021L, 2022L, 2022L),
      age = c(30:31, 31:32)
    )
  )
})

## 'whole_num_halve' ----------------------------------------------------------

test_that("'whole_num_halve' works with even numbers", {
  expect_equal(whole_num_halve(4), 2)
  expect_equal(whole_num_halve(0), 0)
  expect_equal(whole_num_halve(10000), 5000)
})

test_that("'whole_num_halve' works with odd numbers", {
  set.seed(0)
  ans <- replicate(n = 1000, whole_num_halve(5))
  expect_equal(mean(ans), 2.5, tolerance = 0.01)
})
