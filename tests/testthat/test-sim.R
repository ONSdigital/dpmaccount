## 'sim_classif_vars_events' --------------------------------------------------

test_that("'sim_args_estimate' works", {
  ans <- sim_args_estimate()
  expect_identical(names(ans), c("sysmods", "datamods"))
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
