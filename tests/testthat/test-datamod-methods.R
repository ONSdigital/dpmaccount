## 'get_classif_vars' ---------------------------------------------------------

test_that("'get_classif_vars' works with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  classif_vars <- tibble::tibble(classif_vars)
  data <- classif_vars
  data$count <- seq_len(nrow(data))
  mod <- datamod_exact(data, nm_series = "deaths")
  expect_identical(get_classif_vars(mod), classif_vars)
})


## 'get_nm_data' --------------------------------------------------------------

test_that("'get_nm_data' works with valid inputs", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data$count <- seq_len(nrow(data))
  data <- data[-match("triangle", names(data))]
  mod <- datamod_exact(data, nm_series = "deaths")
  expect_identical(get_nm_data(mod), "data")
})


## 'get_nm_series' ------------------------------------------------------------

test_that("'get_nm_series' works with valid inputs", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data$count <- seq_len(nrow(data))
  data <- data[-match("triangle", names(data))]
  mod <- datamod_exact(data, nm_series = "deaths")
  expect_identical(get_nm_series(mod), "deaths")
})


## 'make_codatamod_df' --------------------------------------------------------

test_that("'make_codatamod_df' works with datamod_norm and population", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- 1
  sd <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  sd$sd <- 0.2
  nm_series <- "population"
  nm_data <- "popdata"
  mod <- datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_norm")
  expect_identical(
    validate_codatamod_norm(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
  expect_identical(min(sapply(ans$datamod, function(x) min(x$time))), 1999L)
})

test_that("'make_codatamod_df' works with datamod_norm and events", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data$count <- seq_len(nrow(data))
  data <- data[-match("triangle", names(data))]
  ratio <- 1
  sd <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  sd$cohort <- with(sd, time - age - triangle)
  sd <- sd[, -match("triangle", names(sd))]
  sd$sd <- 0.2
  nm_series <- "ins"
  nm_data <- "insdata"
  mod <- datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_norm")
  expect_identical(
    validate_codatamod_norm(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
})

test_that("'make_codatamod_df' works with datamod_t and population", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- 1
  scale <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  scale$scale <- 0.2
  nm_series <- "population"
  nm_data <- "popdata"
  mod <- datamod_t(
    data = data,
    ratio = ratio,
    scale = scale,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_t")
  expect_identical(
    validate_codatamod_t(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
  expect_identical(min(sapply(ans$datamod, function(x) min(x$time))), 1999L)
})

test_that("'make_codatamod_df' works with datamod_t and events", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data$count <- seq_len(nrow(data))
  data <- data[-match("triangle", names(data))]
  ratio <- 1
  scale <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  scale$scale <- 0.05
  nm_series <- "ins"
  nm_data <- "insdata"
  mod <- datamod_t(
    data = data,
    ratio = ratio,
    scale = scale,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_t")
  expect_identical(
    validate_codatamod_t(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
})

test_that("'make_codatamod_df' works with datamod_nbinom and population", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- 1
  disp <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  disp$disp <- 0.05
  nm_series <- "population"
  nm_data <- "popdata"
  mod <- datamod_nbinom(
    data = data,
    ratio = ratio,
    disp = disp,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_nbinom")
  expect_identical(
    validate_codatamod_nbinom(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
  expect_identical(min(sapply(ans$datamod, function(x) min(x$time))), 1999L)
})

test_that("'make_codatamod_df' works with datamod_nbinom and events", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data$count <- seq_len(nrow(data))
  data <- data[-match("triangle", names(data))]
  ratio <- 1
  disp <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  disp$disp <- 0.05
  nm_series <- "ins"
  nm_data <- "insdata"
  mod <- datamod_nbinom(
    data = data,
    ratio = ratio,
    disp = disp,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_nbinom")
  expect_identical(
    validate_codatamod_nbinom(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
})

test_that("'make_codatamod_df' works with datamod_poisson and population", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- 1
  nm_series <- "population"
  nm_data <- "popdata"
  mod <- datamod_poisson(
    data = data,
    ratio = ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_poisson")
  expect_identical(
    validate_codatamod_poisson(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
  expect_identical(min(sapply(ans$datamod, function(x) min(x$time))), 1999L)
})

test_that("'make_codatamod_df' works with datamod_poisson and events", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data$count <- seq_len(nrow(data))
  data <- data[-match("triangle", names(data))]
  ratio <- 1
  nm_series <- "ins"
  nm_data <- "insdata"
  mod <- datamod_poisson(
    data = data,
    ratio = ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_poisson")
  expect_identical(
    validate_codatamod_poisson(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
})

test_that("'make_codatamod_df' works with datamod_lognorm and population", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- 1
  sd <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  sd$sd <- 0.2
  nm_series <- "population"
  nm_data <- "popdata"
  mod <- datamod_lognorm(
    data = data,
    ratio = ratio,
    sd = sd,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_lognorm")
  expect_identical(
    validate_codatamod_lognorm(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
  expect_identical(min(sapply(ans$datamod, function(x) min(x$time))), 1999L)
})

test_that("'make_codatamod_df' works with datamod_lognorm and events", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data$count <- seq_len(nrow(data))
  data <- data[-match("triangle", names(data))]
  ratio <- 1
  sd <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  sd$cohort <- with(sd, time - age - triangle)
  sd <- sd[, -match("triangle", names(sd))]
  sd$sd <- 0.2
  nm_series <- "ins"
  nm_data <- "insdata"
  mod <- datamod_lognorm(
    data = data,
    ratio = ratio,
    sd = sd,
    nm_series = nm_series,
    nm_data = nm_data
  )
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2003,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  ans <- make_codatamod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "nm_series", "nm_data", "datamod"))
  expect_s3_class(ans$datamod[[1]], "dpmaccount_codatamod_lognorm")
  expect_identical(
    validate_codatamod_lognorm(ans$datamod[[1]]),
    ans$datamod[[1]]
  )
})
