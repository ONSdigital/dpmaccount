## 'estimate_account' ---------------------------------------------------------

test_that("'estimate_account' works with data models for popn, ins, outs", {
  set.seed(0)
  args <- sim_args_estimate()
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with data models for popn only", {
  args <- sim_args_estimate(datamod_ins = FALSE, datamod_outs = FALSE)
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with no data models for popn", {
  args <- sim_args_estimate(datamod_popn = FALSE)
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with no data models popn, ins, outs", {
  args <- sim_args_estimate(
    datamod_popn = FALSE,
    datamod_ins = FALSE,
    datamod_outs = FALSE
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with norm_haspar data model - 1 par", {
  set.seed(0)
  args <- sim_args_estimate()
  dm <- args$datamods[[5]]
  sd <- dm$sd
  args$datamods[[5]] <- datamod_norm(
    data = dm$data,
    ratio = 1,
    sd = sd,
    scale_sd = 0.1,
    nm_series = dm$nm_series,
    nm_data = "data_outs"
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with norm_haspar data model - 2 pars", {
  set.seed(0)
  args <- sim_args_estimate()
  dm <- args$datamods[[5]]
  sd <- dm$sd
  args$datamods[[5]] <- datamod_norm(
    data = dm$data,
    ratio = 1,
    sd = sd,
    scale_ratio = 0.2,
    scale_sd = 0.1,
    nm_series = dm$nm_series,
    nm_data = "data_outs"
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with t data model", {
  set.seed(0)
  args <- sim_args_estimate()
  dm <- args$datamods[[5]]
  scale <- dm$sd
  names(scale)[match("sd", names(scale))] <- "scale"
  args$datamods[[5]] <- datamod_t(
    data = dm$data,
    df = 4,
    ratio = 1,
    scale = scale,
    nm_series = dm$nm_series,
    nm_data = "data_outs"
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with keep_adfun = TRUE", {
  set.seed(0)
  args <- sim_args_estimate()
  args <- c(args, list(keep_adfun = TRUE))
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})
