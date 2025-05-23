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


## 'datamod_t' -------------------------------------------------------------

test_that("'datamod_t' works with valid inputs - ratio is number", {
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

test_that("'datamod_t' works with valid inputs - ratio is data frame", {
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
