## 'add_init_to_classif_vars' -------------------------------------------------

test_that("'add_init_to_classif_vars' works with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans_obtained <- add_init_to_classif_vars(classif_vars)
  extra <- expand.grid(age = 0:2, sex = c("Female", "Male"), time = 1999L)
  extra$cohort <- with(extra, time - age)
  ans_expected <- rbind(classif_vars, extra)
  ans_expected <- ans_expected[
    with(
      ans_expected,
      order(cohort, sex, time, age)
    ),
    c("cohort", "sex", "time", "age")
  ]
  rownames(ans_expected) <- NULL
  expect_identical(ans_expected, ans_obtained)
})


## 'make_classif_vars' --------------------------------------------------------

test_that("'make_classif_vars' works with valid inputs", {
  data <- expand.grid(
    age = 0:4,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data <- data[-match("triangle", names(data))]
  data$count <- 0.1
  datamod_bth <- datamod_exact(data = data, nm_series = "births")
  datamod_dth <- datamod_exact(data = data, nm_series = "deaths")
  datamods <- list(datamod_bth, datamod_dth)
  ans_obtained <- make_classif_vars(datamods)
  ans_expected <- data[-match("count", names(data))]
  ans_expected <- ans_expected[
    with(
      ans_expected,
      order(cohort, sex, time, age)
    ),
    c("cohort", "sex", "time", "age")
  ]
  ans_expected <- tibble::tibble(ans_expected)
  expect_identical(ans_expected, ans_obtained)
})


## 'make_cobthdth_df' ---------------------------------------------------------

test_that("'make_cobthdth_df' works with valid inputs", {
  data_dth <- expand.grid(
    age = 0:4,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data_dth$cohort <- with(data_dth, time - age - triangle)
  data_dth <- data_dth[-match("triangle", names(data_dth))]
  data_dth$count <- 10
  data_dth <- data_dth[with(data_dth, order(cohort, sex, time, age)), ]
  data_bth <- data_dth[data_dth$age %in% 2:3, ]
  data_bth$count <- 15
  datamods <- list(
    datamod_exact(
      data = data_dth,
      nm_series = "deaths"
    ),
    datamod_exact(
      data = data_bth,
      nm_series = "births"
    )
  )
  ans_obtained <- make_cobthdth_df(datamods)
  ans_expected <- data_dth
  names(ans_expected)[match("count", names(ans_expected))] <- "val_dth"
  ans_expected$val_bth <- rep(list(c(Female = NA_real_, Male = NA_real_)),
    times = nrow(ans_expected)
  )
  has_bth <- ans_expected$age %in% 2:3 & ans_expected$sex == "Female"
  ans_expected$has_bth <- 1L * has_bth
  ans_expected$val_bth[has_bth] <- rep(list(c(Female = 15, Male = 15)),
    times = sum(has_bth)
  )
  ans_expected$nm_data_bth <- "data_bth"
  ans_expected$nm_data_dth <- "data_dth"
  ans_expected <- nest_to_df(ans_expected,
    nms_data = c(
      "has_bth", "val_bth", "val_dth",
      "nm_data_bth", "nm_data_dth",
      "time", "age"
    ),
    nms_group = c("cohort", "sex"),
    nm_val = "count_bthdth"
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'make_codatamods_df' -------------------------------------------------------

test_that("'make_codatamods_df' works with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:3,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  data_ins <- classif_vars
  data_outs <- classif_vars
  data_popn <- unique(classif_vars[-match("cohort", names(classif_vars))])
  data_ins$count <- 5
  data_outs$count <- 6
  data_popn$count <- 100
  sd_ins <- data_ins
  sd_outs <- data_outs
  sd_popn <- data_popn
  sd_ins$sd <- 0.1
  sd_outs$sd <- 0.2
  sd_popn$sd <- 0.3
  sd_ins <- sd_ins[-match("count", names(sd_ins))]
  sd_outs <- sd_outs[-match("count", names(sd_outs))]
  sd_popn <- sd_popn[-match("count", names(sd_popn))]
  datamods <- list(
    datamod_norm(
      data = data_ins,
      sd = sd_ins,
      nm_series = "ins",
      nm_data = "data_ins1"
    ),
    datamod_norm(
      data = data_ins,
      sd = sd_ins,
      nm_series = "ins",
      nm_data = "data_ins2"
    ),
    datamod_norm(
      data = data_outs,
      sd = sd_outs,
      nm_series = "outs",
      nm_data = "data_outs"
    ),
    datamod_norm(
      data = data_popn,
      sd = sd_popn,
      nm_series = "population",
      nm_data = "data_popn"
    )
  )
  ans <- make_codatamods_df(
    datamods = datamods,
    classif_vars = classif_vars
  )
  expect_identical(names(ans), c(
    "cohort", "sex", "datamods_stk",
    "datamods_ins", "datamods_outs"
  ))
  expect_true(all(sapply(
    ans[c("datamods_stk", "datamods_ins", "datamods_outs")],
    is.list
  )))
})

test_that("'make_codatamods_df' behaves correctly when a series has no data", {
  classif_vars <- expand.grid(
    age = 0:3,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  data_ins <- classif_vars
  data_outs <- classif_vars
  data_popn <- unique(classif_vars[-match("cohort", names(classif_vars))])
  data_ins$count <- 5
  data_popn$count <- 100
  sd_ins <- data_ins
  sd_popn <- data_popn
  sd_ins$sd <- 0.1
  sd_popn$sd <- 0.3
  sd_ins <- sd_ins[-match("count", names(sd_ins))]
  sd_popn <- sd_popn[-match("count", names(sd_popn))]
  datamods <- list(
    datamod_norm(
      data = data_ins,
      sd = sd_ins,
      nm_series = "ins",
      nm_data = "ins1"
    ),
    datamod_norm(
      data = data_ins,
      sd = sd_ins,
      nm_series = "ins",
      nm_data = "ins2"
    ),
    datamod_norm(
      data = data_popn,
      sd = sd_popn,
      nm_series = "population",
      nm_data = "popn"
    )
  )
  ans <- make_codatamods_df(
    datamods = datamods,
    classif_vars = classif_vars
  )
  expect_identical(names(ans), c(
    "cohort", "sex", "datamods_stk",
    "datamods_ins", "datamods_outs"
  ))
  expect_true(all(sapply(
    ans[c("datamods_stk", "datamods_ins", "datamods_outs")],
    is.list
  )))
  expect_identical(ans[["datamods_outs"]], rep(list(list()), nrow(ans)))
})

test_that("'make_codatamods_df' behaves correctly when a series is missing some categories", {
  classif_vars <- expand.grid(
    age = 0:3,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  data_ins <- classif_vars
  data_outs <- classif_vars
  data_popn <- unique(classif_vars[-match("cohort", names(classif_vars))])
  data_ins$count <- 5
  data_outs$count <- 6
  data_popn$count <- 100
  sd_ins <- data_ins
  sd_outs <- data_outs
  sd_popn <- data_popn
  sd_ins$sd <- 0.1
  sd_outs$sd <- 0.2
  sd_popn$sd <- 0.3
  sd_ins <- sd_ins[-match("count", names(sd_ins))]
  sd_outs <- sd_outs[-match("count", names(sd_outs))]
  sd_popn <- sd_popn[-match("count", names(sd_popn))]
  data_ins <- data_ins[data_ins$sex != "Male", ]
  data_popn <- data_popn[data_popn$sex != "Female", ]
  sd_ins <- sd_ins[sd_ins$sex != "Male", ]
  sd_popn <- sd_popn[sd_popn$sex != "Female", ]
  datamods <- list(
    datamod_norm(
      data = data_ins,
      sd = sd_ins,
      nm_series = "ins",
      nm_data = "data_ins1"
    ),
    datamod_norm(
      data = data_ins,
      sd = sd_ins,
      nm_series = "ins",
      nm_data = "data_ins2"
    ),
    datamod_norm(
      data = data_outs,
      sd = sd_outs,
      nm_series = "outs",
      nm_data = "data_outs"
    ),
    datamod_norm(
      data = data_popn,
      sd = sd_popn,
      nm_series = "population",
      nm_data = "data_popn"
    )
  )
  ans <- make_codatamods_df(
    datamods = datamods,
    classif_vars = classif_vars
  )
  expect_identical(names(ans), c(
    "cohort", "sex", "datamods_stk",
    "datamods_ins", "datamods_outs"
  ))
  expect_true(all(sapply(
    ans[c("datamods_stk", "datamods_ins", "datamods_outs")],
    is.list
  )))
  is_obs <- unlist(ans[ans$sex == "Male", "datamods_ins"], recursive = TRUE)
  is_obs <- is_obs[grep("is_obs", names(is_obs))]
  expect_true(all(!is_obs))
  is_obs <- unlist(ans[ans$sex == "Female", "datamods_stk"], recursive = TRUE)
  is_obs <- is_obs[grep("is_obs", names(is_obs))]
  expect_true(all(!is_obs))
})


## 'make_costkinit_df' --------------------------------------------------------

test_that("'make_costkinit_df' works with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:3,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  data_births <- classif_vars[classif_vars$age %in% 1:2, ]
  data_popn <- add_init_to_classif_vars(classif_vars)
  data_popn <- unique(data_popn[-match("cohort", names(data_popn))])
  data_births$count <- 3
  data_popn$count <- seq_len(nrow(data_popn))
  sd_popn <- data_popn
  sd_popn$sd <- 0.1
  sd_popn <- sd_popn[-match("count", names(sd_popn))]
  datamods <- list(
    datamod_norm(
      data = data_popn,
      sd = sd_popn,
      nm_series = "population"
    ),
    datamod_exact(
      data = data_births,
      nm_series = "births"
    )
  )
  ans <- make_costkinit_df(
    datamods = datamods,
    classif_vars = classif_vars
  )
  expect_identical(names(ans), c("cohort", "sex", "is_new_cohort", "prior_stk_init"))
  expect_identical(names(ans$prior_stk_init[[1]]), c("mean", "sd"))
  expect_identical(nrow(ans), nrow(unique(classif_vars[c("cohort", "sex")])))
})


## 'make_cosysmods_df' --------------------------------------------------------

test_that("'make_cosysmods_df' works with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mean_ins <- unique(classif_vars[c("age", "sex")])
  mean_ins$mean <- 0.1
  mean_outs <- unique(classif_vars[c("age", "time")])
  mean_outs$mean <- 0.2
  mod_ins <- sysmod(
    mean = mean_ins,
    nm_series = "ins"
  )
  mod_outs <- sysmod(
    mean = mean_outs,
    nm_series = "outs"
  )
  sysmods <- list(mod_ins, mod_outs)
  ans <- make_cosysmods_df(sysmods = sysmods, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "sysmod_ins", "sysmod_outs"))
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
