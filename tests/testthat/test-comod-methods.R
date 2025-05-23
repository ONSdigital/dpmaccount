## 'fit' ----------------------------------------------------------------------

test_that("'fit' works with valid data - new cohort", {
  x <- sim_comod(is_new_cohort = TRUE)
  ans <- fit(x)
  expect_s3_class(ans, "dpmaccount_comod")
  expect_true(!is.na(ans$mean[[1]]))
})


test_that("'fit' works with very small new cohort", {
  x <- sim_comod(mean_stk_init = 0.001, is_new_cohort = TRUE)
  x$prior_stk_init$mean <- 0
  ans <- fit(x)
  expect_s3_class(ans, "dpmaccount_comod")
  expect_true(!is.na(ans$mean[[1]]))
})


test_that("'fit' works with valid data - existing cohort", {
  x <- sim_comod(is_new_cohort = FALSE)
  ans <- fit(x)
  expect_s3_class(ans, "dpmaccount_comod")
  expect_true(!is.na(ans$mean[[1]]))
})


## 'get_nm_data_bth' ----------------------------------------------------------

test_that("'get_nm_data_bth' works with dpmaccount_comod", {
  x <- sim_comod()
  expect_identical(get_nm_data_bth(x), "dataset_bth")
})


## 'get_nm_data_dth' ----------------------------------------------------------

test_that("'get_nm_data_dth' works with dpmaccount_comod", {
  x <- sim_comod()
  expect_identical(get_nm_data_dth(x), "dataset_dth")
})


## 'is_at_risk_bth' -----------------------------------------------------------

test_that("'is_at_risk_bth' works with dpmaccount_comod", {
  x <- sim_comod(is_new_cohort = TRUE)
  expect_false(is_at_risk_bth(x))
  x <- sim_comod(is_new_cohort = FALSE)
  expect_true(is_at_risk_bth(x))
})


## 'is_fitted' ----------------------------------------------------------------

test_that("'is_fitted' works with dpmaccount_comod", {
  x <- sim_comod()
  expect_false(is_fitted(x))
  x <- fit(x)
  expect_true(is_fitted(x))
})


## 'summary' ------------------------------------------------------------------

test_that("'summary' works with dpmaccount_comod - all data models present", {
  comod <- sim_comod()
  s <- summary(comod)
  expect_s3_class(s, "dpmaccount_comod_summary")
  expect_true(all(c("dataset_stk", "dataset_ins", "dataset_outs") %in% names(s$data)))
})

test_that("'summary' works with dpmaccount_comod - only stock data model present", {
  comod <- sim_comod()
  comod$datamods_ins <- list()
  comod$datamods_outs <- list()
  s <- summary(comod)
  expect_s3_class(s, "dpmaccount_comod_summary")
  expect_false(all(c("dataset_ins", "dataset_outs") %in% names(s$data)))
})
