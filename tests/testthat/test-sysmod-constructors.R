## 'new_sysmod' ---------------------------------------------------------------

test_that("'new_sysmod' works with valid inputs", {
  mean <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  mean$cohort <- with(mean, time - age - triangle)
  mean <- mean[-match("triangle", names(mean))]
  mean$mean <- seq_len(nrow(mean))
  disp <- 0.1
  nm_series <- "births"
  ans <- new_sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  expect_s3_class(ans, "dpmaccount_sysmod")
})


## 'sysmod' -------------------------------------------------------------------

test_that("'sysmod' works with valid inputs - disp is number", {
  mean <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  mean$cohort <- with(mean, time - age - triangle)
  mean <- mean[-match("triangle", names(mean))]
  mean$mean <- seq_len(nrow(mean))
  disp <- 0.1
  nm_series <- "births"
  ans <- sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  expect_s3_class(ans, "dpmaccount_sysmod")
})

test_that("'sysmod' works with valid inputs - disp is data frame", {
  mean <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  mean$cohort <- with(mean, time - age - triangle)
  mean <- mean[-match("triangle", names(mean))]
  mean$mean <- seq_len(nrow(mean))
  disp <- mean
  disp$disp <- 0.2
  disp <- disp[-match("mean", names(disp))]
  nm_series <- "births"
  ans <- sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  expect_s3_class(ans, "dpmaccount_sysmod")
})

test_that("'sysmod' gives expected error when disp invalid", {
  mean <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  mean$cohort <- with(mean, time - age - triangle)
  mean <- mean[-match("triangle", names(mean))]
  mean$mean <- seq_len(nrow(mean))
  disp <- NULL
  nm_series <- "births"
  expect_error(
    sysmod(
      mean = mean,
      disp = disp,
      nm_series = nm_series
    ),
    "'disp' has class \"NULL\""
  )
})
