## 'as_tibble' ----------------------------------------------------------------

test_that("'as_tibble' works", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans_obtained <- tibble::as_tibble(x)
  ans_expected <- tibble::tibble(
    cohort = x$cohort,
    sex = x$sex,
    fitted = x$fitted
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'components' ---------------------------------------------------------------

test_that("'components' works when what is population", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- components(x, n_draw = 5)
  expect_setequal(ans$age, args$datamods[[1]]$data$age)
  expect_setequal(ans$sex, args$datamods[[1]]$data$sex)
  expect_setequal(ans$cohort, args$datamods[[1]]$data$cohort)
  expect_setequal(ans$time, args$datamods[[1]]$data$time)
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "population", "population.fitted",
      "population.lower", "population.upper"
    )
  )
})

test_that("'components' works when what is population - need to adjust popn", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  x$fitted[[1]]$mean[] <- c(1, 3, 4)
  ans <- components(x, n_draw = 500)
  expect_identical(names(ans), c("population", "propn_adjusted"))
})

test_that("'components' works when what is 'events'", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "events",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "births", "deaths", "ins", "outs",
      "ins.fitted", "ins.lower", "ins.upper",
      "outs.fitted", "outs.lower", "outs.upper"
    )
  )
})

test_that("'components' works when what is 'exposure'", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "exposure",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c("age", "sex", "cohort", "time", "exposure")
  )
})

test_that("'components' works when what is 'rates'", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "rates",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "births", "deaths", "ins", "outs",
      "births.fitted", "births.lower", "births.upper",
      "deaths.fitted", "deaths.lower", "deaths.upper",
      "ins.fitted", "ins.lower", "ins.upper",
      "outs.fitted", "outs.lower", "outs.upper"
    )
  )
})

test_that("'components' works when what is 'sysmods'", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = "sysmods",
    n_draw = 5
  )
  expect_setequal(
    names(ans),
    c(
      "age", "sex", "cohort", "time",
      "births.mean", "births.disp",
      "deaths.mean", "deaths.disp",
      "ins.mean", "ins.disp",
      "outs.mean", "outs.disp"
    )
  )
})

test_that("'components' works when what is data_population, data_events", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = c("data_population", "data_events"),
    n_draw = 5
  )
  expect_true(is.list(ans))
  expect_identical(names(ans), c("data_population", "data_events"))
  expect_setequal(
    names(ans$data_population),
    c("age", "sex", "cohort", "time", "data_popn")
  )
  expect_setequal(
    names(ans$data_events),
    c("age", "sex", "cohort", "time", "data_births", "data_deaths", "data_ins", "data_outs")
  )
})

test_that("'components' works when what is everything", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models(scale_sd = 0.1))
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = c(
      "population", "events", "exposure", "rates",
      "datamods", "sysmods", "data_population", "data_events"
    ),
    n_draw = 5
  )
  expect_true(is.list(ans))
  expect_setequal(names(ans), c(
    "population", "events", "exposure", "rates", "sysmods",
    "datamods", "data_population", "data_events"
  ))
})

test_that("'components' works collapse is 'cohort'", {
  set.seed(0)
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- components(x,
    what = c(
      "population", "events", "exposure", "rates",
      "sysmods", "data_population", "data_events"
    ),
    n_draw = 5,
    collapse = "cohort"
  )
  expect_true(is.list(ans))
  expect_setequal(names(ans), c(
    "population", "events", "exposure", "rates", "sysmods",
    "data_population", "data_events"
  ))
  expect_true(all(sapply(ans, function(x) !("cohort" %in% names(x)))))
})



## 'diagnostics' --------------------------------------------------------------

test_that("'diagnostics' works", {
  set.seed(0)
  # args <- sim_args_estimate()
  args <- list(sysmods = sim_arg_system_models(), datamods = sim_arg_data_models())
  x <- do.call(estimate_account, args)
  ans <- diagnostics(x)
  expect_true(tibble::is_tibble(ans))
})



## 'summary' ------------------------------------------------------------------

test_that("summary.dpmaccount_results works, including printing", {
  set.seed(0)
  sysmod_births <- sysmod(
    mean = gl_sysmod_mean_births,
    disp = 0.2,
    nm_series = "births"
  )
  sysmod_deaths <- sysmod(
    mean = gl_sysmod_mean_deaths,
    disp = 0.2,
    nm_series = "deaths"
  )
  sysmod_ins <- sysmod(
    mean = gl_sysmod_mean_immig,
    disp = 0.2,
    nm_series = "ins"
  )
  sysmod_outs <- sysmod(
    mean = gl_sysmod_mean_emig,
    disp = 0.2,
    nm_series = "outs"
  )
  sysmods <- list(
    sysmod_births,
    sysmod_deaths,
    sysmod_ins,
    sysmod_outs
  )
  datamod_popn <- datamod_norm(
    data = gl_report_popn,
    sd = gl_cover_sd_popn,
    nm_series = "population"
  )
  datamod_births <- datamod_exact(
    data = gl_report_births,
    nm_series = "births"
  )
  datamod_deaths <- datamod_exact(
    data = gl_report_deaths,
    nm_series = "deaths"
  )
  datamod_ins <- datamod_norm(
    data = gl_report_immig,
    sd = gl_cover_sd_immig,
    nm_series = "ins"
  )
  datamod_outs <- datamod_norm(
    data = gl_report_emig,
    sd = gl_cover_sd_emig,
    nm_series = "outs"
  )
  datamods <- list(
    datamod_popn = datamod_popn,
    datamod_births = datamod_births,
    datamod_deaths = datamod_deaths,
    datamod_ins = datamod_ins,
    datamod_outs = datamod_outs
  )
  results <- estimate_account(
    sysmods = sysmods,
    datamods = datamods
  )
  ans <- summary(results)
  expect_s3_class(ans, "dpmaccount_results_summary")
  expect_identical(
    names(ans),
    c("population", "events", "rates", "n_cohort", "n_success")
  )
  expect_false(anyNA(unlist(ans)))
})



## 'post_pred.dpmaccount_results' ------------------------------------------------------------------

test_that("summary.dpmaccount_results works, including printing", {
  set.seed(0)
  sysmod_births <- sysmod(
    mean = gl_sysmod_mean_births,
    disp = 0.2,
    nm_series = "births"
  )
  sysmod_deaths <- sysmod(
    mean = gl_sysmod_mean_deaths,
    disp = 0.2,
    nm_series = "deaths"
  )
  sysmod_ins <- sysmod(
    mean = gl_sysmod_mean_immig,
    disp = 0.2,
    nm_series = "ins"
  )
  sysmod_outs <- sysmod(
    mean = gl_sysmod_mean_emig,
    disp = 0.2,
    nm_series = "outs"
  )
  sysmods <- list(
    sysmod_births,
    sysmod_deaths,
    sysmod_ins,
    sysmod_outs
  )
  datamod_popn <- datamod_norm(
    data = gl_report_popn,
    sd = gl_cover_sd_popn,
    nm_series = "population"
  )
  datamod_births <- datamod_exact(
    data = gl_report_births,
    nm_series = "births"
  )
  datamod_deaths <- datamod_exact(
    data = gl_report_deaths,
    nm_series = "deaths"
  )
  datamod_ins <- datamod_norm(
    data = gl_report_immig,
    sd = gl_cover_sd_immig,
    nm_series = "ins"
  )
  datamod_outs <- datamod_norm(
    data = gl_report_emig,
    sd = gl_cover_sd_emig,
    nm_series = "outs"
  )
  datamods <- list(
    datamod_popn = datamod_popn,
    datamod_births = datamod_births,
    datamod_deaths = datamod_deaths,
    datamod_ins = datamod_ins,
    datamod_outs = datamod_outs
  )
  results <- estimate_account(
    sysmods = sysmods,
    datamods = datamods
  )

  augment_pop <- augment_population(results, n_draw = 2)
  augment_event <- augment_events(results, n_draw = 2)

  post_pred <- post_pred.dpmaccount_results(results, n_draw = 2)

  #
  expect_false(anyNA(unlist(post_pred$post_pred_events$gl_report_immig_tilde)))
  expect_false(anyNA(unlist(post_pred$post_pred_events$gl_report_emig_tilde)))
  expect_false(anyNA(unlist(post_pred$post_pred_population$gl_report_popn_tilde)))

  #
  expect_false(any(unlist(post_pred$post_pred_events$gl_report_immig_tilde) <= -1e-15))
  expect_false(any(unlist(post_pred$post_pred_events$gl_report_emig_tilde) <= -1e-15))
  expect_false(any(unlist(post_pred$post_pred_population$gl_report_popn_tilde) <= -1e-15))

  expect_true(nrow(post_pred$post_pred_events) == nrow(augment_event))
  expect_true(nrow(post_pred$post_pred_population) == nrow(augment_pop))

  expect_true(all(unlist(lapply(post_pred$post_pred_events$gl_report_immig_tilde, function(x) length(x[[1]]))) == 2))
  expect_true(all(unlist(lapply(post_pred$post_pred_events$gl_report_emig_tilde, function(x) length(x[[1]]))) == 2))
  expect_true(all(unlist(lapply(post_pred$post_pred_population$gl_report_popn_tilde, function(x) length(x[[1]]))) == 1))
})
