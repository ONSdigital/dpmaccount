## 'estimate_account' ---------------------------------------------------------

test_that("'estimate_account' works with data models for popn, ins, outs", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with data models for popn only", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(
    datamod_ins = FALSE,
    datamod_outs = FALSE
  ))
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with no data models for popn", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(datamod_popn = FALSE))
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with no data models popn, ins, outs", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(
    datamod_popn = FALSE,
    datamod_ins = FALSE,
    datamod_outs = FALSE
  ))
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with norm_haspar data model - 1 par", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
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
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
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

test_that("'estimate_account' works with t data model - no pars", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  dm <- args$datamods[[5]]
  scale <- dm$sd
  names(scale)[match("sd", names(scale))] <- "scale"
  args$datamods[[5]] <- datamod_t(
    data = dm$data,
    df = 4,
    ratio = 1,
    scale = scale,
    scale_ratio = 0,
    nm_series = dm$nm_series,
    nm_data = "data_outs"
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})


test_that("'estimate_account' works with t data model - has par", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  dm <- args$datamods[[5]]
  scale <- dm$sd
  names(scale)[match("sd", names(scale))] <- "scale"
  args$datamods[[5]] <- datamod_t(
    data = dm$data,
    df = 4,
    ratio = 1,
    scale = scale,
    scale_ratio = 0.1,
    nm_series = dm$nm_series,
    nm_data = "data_outs"
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with nbinom data model - has par", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  dm <- args$datamods[[5]]
  args$datamods[[5]] <- datamod_nbinom(
    data = dm$data,
    ratio = 1,
    disp = 1,
    scale_ratio = 0.2,
    nm_series = dm$nm_series,
    nm_data = "data_outs"
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with nbinom data model - no par", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  dm <- args$datamods[[5]]
  args$datamods[[5]] <- datamod_nbinom(
    data = dm$data,
    ratio = 1,
    disp = 1,
    scale_ratio = 0,
    nm_series = dm$nm_series,
    nm_data = "data_outs"
  )
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})

test_that("'estimate_account' works with keep_adfun = TRUE", {
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(), seed_in = 0)
  args <- c(args, list(keep_adfun = TRUE))
  ans <- do.call(estimate_account, args)
  expect_s3_class(ans, "dpmaccount_results")
})
