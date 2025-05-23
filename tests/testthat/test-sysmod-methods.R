## 'get_classif_vars' ------------------------------------------------------------

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
  mean <- classif_vars
  mean$mean <- seq_len(nrow(mean))
  disp <- 0.1
  nm_series <- "births"
  mod <- sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  expect_identical(get_classif_vars(mod), classif_vars)
})


## 'get_nm_series' ------------------------------------------------------------

test_that("'get_nm_series' works with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mean <- classif_vars
  mean$mean <- seq_len(nrow(mean))
  disp <- 0.1
  nm_series <- "births"
  mod <- sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  expect_identical(get_nm_series(mod), nm_series)
})


## 'make_cosysmod_df' ---------------------------------------------------------

test_that("'make_cosysmod_df' works with valid inputs - disp is number, sysmod has all vars", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mean <- classif_vars
  mean$mean <- seq_len(nrow(mean))
  disp <- 0.1
  nm_series <- "births"
  mod <- sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  ans <- make_cosysmod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "sysmod_bth"))
  expect_identical(names(ans$sysmod_bth[[1L]]), c("mean", "disp", "time", "age"))
})

test_that("'make_cosysmod_df' works with valid inputs - disp is data frame, sysmod has partial vars", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2002,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mean <- unique(classif_vars[c("age", "sex")])
  mean$mean <- seq_len(nrow(mean))
  disp <- unique(classif_vars[c("age", "sex")])
  disp$disp <- 0.2
  nm_series <- "ins"
  mod <- sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  ans <- make_cosysmod_df(mod = mod, classif_vars = classif_vars)
  expect_identical(names(ans), c("cohort", "sex", "sysmod_ins"))
  expect_identical(names(ans$sysmod_ins[[1L]]), c("mean", "disp", "time", "age"))
})
