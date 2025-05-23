## 'datamod_exact' ------------------------------------------------------------

test_that("'datamod_exact' works with valid inputs", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data <- data[-match("triangle", names(data))]
  data$count <- seq_len(nrow(data))
  nm_series <- "births"
  nm_data <- "birthdata"
  ans <- datamod_exact(
    data = data,
    nm_series = nm_series
  )
  expect_s3_class(ans, "dpmaccount_datamod_exact")
  expect_s3_class(ans, "dpmaccount_datamod")
  expect_identical(ans$nm_data, "data")
})

test_that("'datamod_exact' fails with invalid inputs - sex coding is invalid (male/female)", {
  data <- expand.grid(
    age = 0:2,
    sex = c("female", "male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data <- data[-match("triangle", names(data))]
  data$count <- seq_len(nrow(data))
  nm_series <- "births"
  nm_data <- "birthdata"
  expect_error(
    datamod_exact(
      data = data,
      nm_series = nm_series
    ),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})


## 'datamod_norm' -------------------------------------------------------------

test_that("'datamod_norm' works with valid inputs - ratio is number, scale_ratio, scale_sd both 0", {
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
  ans <- datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_norm")
})

test_that("'datamod_norm' works with valid inputs - ratio is number, scale_ratio, scale_sd both non-zero", {
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
  ans <- datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    scale_ratio = 0.2,
    scale_sd = 0.1,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_norm")
})

test_that("'datamod_norm' works with valid inputs - ratio is data frame", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  ratio$ratio <- 1.3
  sd <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  sd$sd <- 0.2
  nm_series <- "population"
  ans <- datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    nm_series = nm_series
  )
  expect_s3_class(ans, "dpmaccount_datamod_norm")
  expect_identical(ans$nm_data, "data")
})

test_that("'datamod_norm' gives expected error when ratio invalid", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- NULL
  sd <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  sd$sd <- 0.2
  nm_series <- "population"
  expect_error(
    datamod_norm(
      data = data,
      ratio = ratio,
      sd = sd,
      nm_series = nm_series
    ),
    "'ratio' has class \"NULL\""
  )
})

test_that("'datamod_norm' gives expected error when sd non-positive", {
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
  sd$sd[3] <- 0
  nm_series <- "population"
  expect_error(
    datamod_norm(
      data = data,
      ratio = ratio,
      sd = sd,
      nm_series = nm_series
    ),
    "non-positive value \\[0\\] for variable 'sd' : row 3 of data frame 'sd' in data model for dataset \"data\""
  )
})

test_that("'datamod_norm' gives expected error with invalid inputs - invalid 'sex' coding", {
  data <- expand.grid(
    age = 0:2,
    sex = c("female", "male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- 1
  sd <- expand.grid(
    age = 0:2,
    sex = c("female", "male"),
    time = 2000:2002
  )
  sd$sd <- 0.2
  nm_series <- "population"
  nm_data <- "popdata"
  expect_error(
    datamod_norm(
      data = data,
      ratio = ratio,
      sd = sd,
      scale_ratio = 0.2,
      scale_sd = 0.1,
      nm_series = nm_series,
      nm_data = nm_data
    ),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})


## 'datamod_t' -------------------------------------------------------------

test_that("'datamod_t' works with valid inputs - ratio is number, scale_ratio is 0 (default)", {
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
  ans <- datamod_t(
    data = data,
    df = 4,
    ratio = ratio,
    scale = scale,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_t")
})

test_that("'datamod_t' works with valid inputs - ratio is number, scale_ratio is non-zero", {
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
  ans <- datamod_t(
    data = data,
    df = 4,
    ratio = ratio,
    scale = scale,
    scale_ratio = 0.2,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_t")
})

test_that("'datamod_t' works with valid inputs - ratio is data frame, scale_ratio is 0 (default)", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  ratio$ratio <- 1.3
  scale <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  scale$scale <- 0.2
  nm_series <- "population"
  ans <- datamod_t(
    data = data,
    df = 4,
    ratio = ratio,
    scale = scale,
    nm_series = nm_series
  )
  expect_s3_class(ans, "dpmaccount_datamod_t")
  expect_identical(ans$nm_data, "data")
})

test_that("'datamod_t' gives expected error when ratio invalid", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- NULL
  scale <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002
  )
  scale$scale <- 0.2
  nm_series <- "population"
  expect_error(
    datamod_t(
      data = data,
      df = 4,
      ratio = ratio,
      scale = scale,
      nm_series = nm_series
    ),
    "'ratio' has class \"NULL\""
  )
})

test_that("'datamod_t' gives expected error when scale non-positive", {
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
  scale$scale[3] <- 0
  nm_series <- "population"
  expect_error(
    datamod_t(
      data = data,
      df = 4,
      ratio = ratio,
      scale = scale,
      nm_series = nm_series
    ),
    "non-positive value \\[0\\] for variable 'scale' : row 3 of data frame 'scale' in data model for dataset \"data\""
  )
})

test_that("'datamod_t' gives expected error with invalid inputs - invalid 'sex' coding", {
  data <- expand.grid(
    age = 0:2,
    sex = c("female", "male"),
    time = 2000:2002
  )
  data$count <- seq_len(nrow(data))
  ratio <- 1
  scale <- expand.grid(
    age = 0:2,
    sex = c("female", "male"),
    time = 2000:2002
  )
  scale$scale <- 0.2
  nm_series <- "population"
  nm_data <- "popdata"
  expect_error(
    datamod_t(
      data = data,
      df = 4,
      ratio = ratio,
      scale = scale,
      nm_series = nm_series,
      nm_data = nm_data
    ),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})


## 'datamod_nbinom' -------------------------------------------------------------

test_that("'datamod_nbinom' creates object of class 'dpmaccount_datamod_nbinom' - ratio and disp both 1,
          scale_ratio is 0 (default)", {
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
  ans <- datamod_nbinom(
    data = data,
    nm_series = "ins",
    nm_data = "reg_immig"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_nbinom", "dpmaccount_datamod"))
})

test_that("'datamod_nbinom' creates object of class 'dpmaccount_datamod_nbinom' - ratio and disp both 1,
          scale_ratio is non-zero", {
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
  ans <- datamod_nbinom(
    data = data,
    scale_ratio = 0.1,
    nm_series = "ins",
    nm_data = "reg_immig"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_nbinom", "dpmaccount_datamod"))
})

test_that("'datamod_nbinom' creates object of class 'datamod_nbinom' - ratio and disp both df", {
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
  ratio <- data.frame(
    sex = c("Female", "Male"),
    ratio = c(0, 0.5)
  )
  disp <- within(data, {
    rm(count)
    disp <- 0
  })
  ans <- datamod_nbinom(
    data = data,
    ratio = ratio,
    disp = disp,
    nm_series = "ins",
    nm_data = "reg_immig"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_nbinom", "dpmaccount_datamod"))
})

test_that("'datamod_nbinom' creates 'nm_data' when not supplied", {
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
  ans <- datamod_nbinom(
    data = data,
    nm_series = "ins"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_nbinom", "dpmaccount_datamod"))
  expect_identical(ans$nm_data, "data")
})

test_that("'datamod_nbinom' creates object of class 'dpmaccount_datamod_nbinom' - only 'Female' coding", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  classif_vars <- tibble::tibble(classif_vars)
  data <- classif_vars
  data$count <- seq_len(nrow(data))
  ans <- datamod_nbinom(
    data = data,
    nm_series = "ins",
    nm_data = "reg_immig"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_nbinom", "dpmaccount_datamod"))
})


test_that("'datamod_nbinom' gives expected error with invalid inputs - invalid 'sex' coding", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("female", "male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  classif_vars <- tibble::tibble(classif_vars)
  data <- classif_vars
  data$count <- seq_len(nrow(data))
  expect_error(
    datamod_nbinom(
      data = data,
      nm_series = "ins",
      nm_data = "reg_immig"
    ),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})


## 'new_datamod_exact' --------------------------------------------------------

test_that("'new_datamod_exact' works with valid inputs", {
  data <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  data$cohort <- with(data, time - age - triangle)
  data <- data[-match("triangle", names(data))]
  data$count <- seq_len(nrow(data))
  nm_series <- "births"
  nm_data <- "birthdata"
  ans <- new_datamod_exact(
    data = data,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_exact")
  expect_s3_class(ans, "dpmaccount_datamod")
})


## 'new_datamod_norm' ---------------------------------------------------------

test_that("'new_datamod_norm' works with valid inputs - scale_ratio, scale_sd both 0", {
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
  ans <- new_datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    scale_ratio = 0,
    scale_sd = 0,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_norm")
  expect_s3_class(ans, "dpmaccount_datamod")
})

test_that("'new_datamod_norm' works with valid inputs - scale_ratio, scale_sd both non-zero", {
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
  ans <- new_datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    scale_ratio = 0.2,
    scale_sd = 2,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_norm")
  expect_s3_class(ans, "dpmaccount_datamod")
})


## 'new_datamod_t' ---------------------------------------------------------

test_that("'new_datamod_t' works with valid inputs - scale_ratio equal 0 (default)", {
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
  ans <- new_datamod_t(
    data = data,
    df = 4,
    ratio = ratio,
    scale = scale,
    scale_ratio = 0,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_t")
  expect_s3_class(ans, "dpmaccount_datamod")
})

test_that("'new_datamod_t' works with valid inputs - scale_ratio non-zero", {
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
  ans <- new_datamod_t(
    data = data,
    df = 4,
    ratio = ratio,
    scale = scale,
    scale_ratio = 0.2,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_t")
  expect_s3_class(ans, "dpmaccount_datamod")
})


## new_datamod_nbinom --------------------------------------------------------------

test_that("'new_datamod_nbinom' creates object of class 'dpmaccount_datamod_nbinom
          - scale_ratio equal 0 (default)", {
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
  ratio <- within(data, {
    rm(count)
    ratio <- 1
  })
  disp <- within(data, {
    rm(count)
    disp <- 0
  })
  nm_series <- "ins"
  nm_data <- "immig"
  ans <- new_datamod_nbinom(
    data = data,
    ratio = ratio,
    disp = disp,
    scale_ratio = 0,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_nbinom")
  expect_s3_class(ans, "dpmaccount_datamod")
})

test_that("'new_datamod_nbinom' creates object of class 'dpmaccount_datamod_nbinom
          - scale_ratio non-zero", {
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
  ratio <- within(data, {
    rm(count)
    ratio <- 1
  })
  disp <- within(data, {
    rm(count)
    disp <- 0
  })
  nm_series <- "ins"
  nm_data <- "immig"
  ans <- new_datamod_nbinom(
    data = data,
    ratio = ratio,
    disp = disp,
    scale_ratio = 0.2,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_nbinom")
  expect_s3_class(ans, "dpmaccount_datamod")
})


## new_datamod_poisson --------------------------------------------------------------

test_that("'new_datamod_poisson' creates object of class 'dpmaccount_datamod_poisson,
          ratio is data frame all values unique, scale_ratio is 0 (default) ", {
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
  ratio <- within(data, {
    ratio <- count
    rm(count)
  })
  nm_series <- "ins"
  nm_data <- "immig"
  ans <- new_datamod_poisson(
    data = data,
    ratio = ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_poisson")
  expect_s3_class(ans, "dpmaccount_datamod")
})

test_that("'new_datamod_poisson' creates object of class 'dpmaccount_datamod_poisson,
          ratio is data frame all values unique, scale_ratio is non-zero ", {
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
  ratio <- within(data, {
    ratio <- count
    rm(count)
  })
  nm_series <- "ins"
  nm_data <- "immig"
  ans <- new_datamod_poisson(
    data = data,
    ratio = ratio,
    scale_ratio = 0.1,
    nm_series = nm_series,
    nm_data = nm_data
  )
  expect_s3_class(ans, "dpmaccount_datamod_poisson")
  expect_s3_class(ans, "dpmaccount_datamod")
})

test_that("'datamod_poisson' creates object of class 'dpmaccount_datamod_poisson' - ratio equal 1, scale_ratio 0 (both default)", {
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
  ans <- datamod_poisson(
    data = data,
    nm_series = "ins",
    nm_data = "immig"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_poisson", "dpmaccount_datamod"))
})

test_that("'datamod_poisson' creates object of class 'dpmaccount_datamod_poisson
          ' - ratio equal 1 (default), scale_ratio non-zero", {
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
  ans <- datamod_poisson(
    data = data,
    scale_ratio = 0.2,
    nm_series = "ins",
    nm_data = "immig"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_poisson", "dpmaccount_datamod"))
})


test_that("'datamod_poisson' creates object of class 'datamod_poisson' -
          ratio is dataframe of different size to data, scale_ratio is 0 (default)", {
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
  ratio <- data.frame(
    sex = c("Female", "Male"),
    ratio = c(0, 0.5)
  )
  ans <- datamod_poisson(
    data = data,
    ratio = ratio,
    nm_series = "ins",
    nm_data = "reg_immig"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_poisson", "dpmaccount_datamod"))
})

test_that("'datamod_poisson' creates 'nm_data' when not supplied", {
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
  ans <- datamod_poisson(
    data = data,
    nm_series = "ins"
  )
  expect_s3_class(ans, c("dpmaccount_datamod_poisson", "dpmaccount_datamod"))
  expect_identical(ans$nm_data, "data")
})
