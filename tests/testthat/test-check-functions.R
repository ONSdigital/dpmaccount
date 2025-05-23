## 'check_age' ----------------------------------------------------------------

test_that("'check_age' returns TRUE with valid inputs", {
  df <- data.frame(age = 0:10)
  expect_true(check_age(df, nm_df = "data"))
})

test_that("'check_age' returns correct error with invalid inputs", {
  df <- data.frame(age = 0.5)
  expect_error(
    check_age(df, nm_df = "data"),
    "problem with variable 'age' in data frame 'data' :"
  )
})


## 'check_age_complete' -------------------------------------------------------

test_that("'check_age_complete' returns TRUE with valid inputs - is births", {
  df <- data.frame(age = 0:10)
  expect_true(check_age_complete(df, nm_df = "data", is_births = FALSE))
})

test_that("'check_age_complete' returns TRUE with valid inputs - is not births", {
  df <- data.frame(age = 5:10)
  expect_true(check_age_complete(df, nm_df = "data", is_births = TRUE))
})

test_that("'check_age_complete' raises correct error when min age not 0", {
  df <- data.frame(age = 1:10)
  expect_error(
    check_age_complete(df, nm_df = "data", is_births = FALSE),
    "minimum value for 'age' in data frame 'data' is 1"
  )
})

test_that("'check_age_complete' raises correct error when missing intermediate level", {
  df <- data.frame(age = c(1:10, 12:20))
  expect_error(
    check_age_complete(df, nm_df = "data", is_births = TRUE),
    "'age' variable in data frame 'data' is missing value '11'"
  )
})


## 'check_and_tidy_nm_series' -------------------------------------------------

test_that("'check_and_tidy_nm_series' returns string with valid inputs", {
  expect_identical(
    check_and_tidy_nm_series("pop"),
    "population"
  )
  expect_identical(
    check_and_tidy_nm_series("b"),
    "births"
  )
  expect_identical(
    check_and_tidy_nm_series("d"),
    "deaths"
  )
  expect_identical(
    check_and_tidy_nm_series("in"),
    "ins"
  )
  expect_identical(
    check_and_tidy_nm_series("out"),
    "outs"
  )
})

test_that("'check_and_tidy_nm_series raises error with invalid inputs", {
  expect_error(check_and_tidy_nm_series("wrong"))
})


## 'check_classif_complete' ---------------------------------------------------

test_that("'check_classif_complete' returns TRUE with valid inputs - no cohort", {
  df <- expand.grid(age = 0:3, sex = c("F", "M"), time = 2000:2001)
  expect_true(check_classif_complete(df, nm_df = "data"))
})

test_that("'check_classif_complete' returns TRUE with valid inputs - with cohort", {
  df <- expand.grid(age = 0:3, sex = c("F", "M"), time = 2000:2001, cohort = 1995:2001)
  df <- subset(df, (time - cohort - age) %in% 0:1)
  expect_true(check_classif_complete(df, nm_df = "data"))
})

test_that("'check_classif_complete' returns TRUE with valid inputs - with cohort, births", {
  df <- expand.grid(age = 1:3, sex = c("F", "M"), time = 2000:2001, cohort = 1995:2000)
  df <- subset(df, (time - cohort - age) %in% 0:1)
  expect_true(check_classif_complete(df, nm_df = "data"))
})

test_that("'check_classif_complete' returns correct error with ivalid inputs", {
  df <- expand.grid(age = 0:3, sex = c("F", "M"), time = 2000:2001, cohort = 1995:2001)
  df <- subset(df, (time - cohort - age) %in% 0:1)
  df <- df[-1, ]
  expect_error(
    check_classif_complete(df, nm_df = "data"),
    "data frame 'data' does not include all possible combinations of classification variables"
  )
})


## 'check_classif_no_dup' -----------------------------------------------------

test_that("'check_classif_no_dup' returns TRUE with valid inputs", {
  df <- expand.grid(age = 0:3, sex = c("F", "M"), time = 2000:2001)
  expect_true(check_classif_no_dup(df, nm_df = "data"))
})

test_that("'check_classif_no_dup' returns TRUE with valid inputs", {
  df <- expand.grid(age = 0:3, sex = c("F", "M"), time = 2000:2001)
  expect_error(
    check_classif_no_dup(rbind(df, df), nm_df = "data"),
    paste(
      "classification variables for data frame 'data' have two rows",
      "with the following values : '0', 'F', '2000'"
    )
  )
})


## 'check_cohort' -------------------------------------------------------------

test_that("'check_cohort' returns TRUE with valid inputs", {
  df <- data.frame(cohort = 0:10)
  expect_true(check_cohort(df, nm_df = "data"))
})

test_that("'check_cohort' returns correct error with invalid inputs", {
  df <- data.frame(cohort = 0.5)
  expect_error(
    check_cohort(df, nm_df = "data"),
    "problem with variable 'cohort' in data frame 'data' :"
  )
})


## 'check_cohort_consistent' --------------------------------------------------

test_that("'check_cohort_consistent' returns TRUE with valid inputs", {
  df <- data.frame(
    age = rep(0:1, each = 4),
    time = rep(2000:2001, each = 2),
    cohort = c(
      1999, 2000,
      2000, 2001,
      1998, 1999,
      1999, 2000
    )
  )
  expect_true(check_cohort_consistent(df, nm_df = "data"))
})

test_that("'check_cohort_consistent' returns expected error with invalid inputs", {
  df <- data.frame(
    age = rep(0:1, each = 4),
    time = rep(2000:2001, each = 2),
    cohort = c(
      1999, 2000,
      2000, 2001,
      1998, 1999,
      1999, 2010
    )
  )
  expect_error(
    check_cohort_consistent(df, nm_df = "data"),
    paste(
      "'age' \\[1\\], 'time' \\[2001\\], and 'cohort' \\[2010] in row 8",
      "of data frame 'data' are inconsistent"
    )
  )
})


## 'check_collapse' -----------------------------------------------------------

test_that("'check_collapse' returns TRUE with valid inputs", {
  expect_true(check_collapse(NULL, can_collapse_time = TRUE))
  expect_true(check_collapse("age", can_collapse_time = TRUE))
  expect_true(check_collapse(c("age", "sex", "cohort"),
    can_collapse_time = TRUE
  ))
  expect_true(check_collapse(c("age", "sex", "time"),
    can_collapse_time = TRUE
  ))
})

test_that("'check_collapse' returns expected error with invalid inputs", {
  expect_error(
    check_collapse("wrong", can_collapse_time = TRUE),
    "'wrong' is not a valid value for 'collapse'"
  )
  expect_error(
    check_collapse("time", can_collapse_time = FALSE),
    "'time' is not a valid value for 'collapse'"
  )
  expect_error(
    check_collapse(c("age", "time", "cohort"), can_collapse_time = TRUE),
    paste(
      "cannot simultaneously collapse \"age\", \"cohort\",",
      "and \"time\" : need to retain at least one of the three"
    )
  )
})


## 'check_datamods' -----------------------------------------------------------

test_that("'check_datamods' returns TRUE with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mod_bth <- datamod_exact(
    data = within(classif_vars, {
      count <- 2
    }),
    nm_data = "data1",
    nm_series = "births"
  )
  mod_dth <- datamod_exact(
    data = within(classif_vars, {
      count <- 3
    }),
    nm_data = "data2",
    nm_series = "deaths"
  )
  mod_ins <- datamod_norm(
    data = within(classif_vars, {
      count <- 1.2
    }),
    nm_data = "data3",
    sd = within(classif_vars, {
      sd <- 1.2
    }),
    nm_series = "ins"
  )
  mod_outs <- datamod_norm(
    data = within(classif_vars, {
      count <- 1.2
    }),
    sd = within(classif_vars, {
      sd <- 1.2
    }),
    nm_data = "data4",
    nm_series = "outs"
  )
  datamods <- list(mod_bth, mod_dth, mod_ins, mod_outs)
  expect_true(check_datamods(datamods))
})

test_that("'check_datamods' returns correct error with duplicated 'nm_data'", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mod_bth <- datamod_exact(
    data = within(classif_vars, {
      count <- 2
    }),
    nm_data = "data1",
    nm_series = "births"
  )
  mod_dth <- datamod_exact(
    data = within(classif_vars, {
      count <- 3
    }),
    nm_data = "data2",
    nm_series = "deaths"
  )
  mod_ins <- datamod_norm(
    data = within(classif_vars, {
      count <- 1.2
    }),
    nm_data = "data3",
    sd = within(classif_vars, {
      sd <- 1.2
    }),
    nm_series = "ins"
  )
  mod_outs <- datamod_norm(
    data = within(classif_vars, {
      count <- 1.2
    }),
    sd = within(classif_vars, {
      sd <- 1.2
    }),
    nm_data = "data3",
    nm_series = "outs"
  )
  datamods <- list(mod_bth, mod_dth, mod_ins, mod_outs)
  expect_error(
    check_datamods(datamods),
    "two data models with the same value for 'nm_data' \\[\"data3\"\\]"
  )
})

test_that("'check_datamods' returns correct errors with wrong number of births/deaths series'", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mod_bth <- datamod_exact(
    data = within(classif_vars, {
      count <- 2
    }),
    nm_data = "data1",
    nm_series = "births"
  )
  mod_dth <- datamod_exact(
    data = within(classif_vars, {
      count <- 3
    }),
    nm_data = "data2",
    nm_series = "deaths"
  )
  mod_ins <- datamod_norm(
    data = within(classif_vars, {
      count <- 1.2
    }),
    nm_data = "data3",
    sd = within(classif_vars, {
      sd <- 1.2
    }),
    nm_series = "ins"
  )
  mod_bth2 <- datamod_exact(
    data = within(classif_vars, {
      count <- 1.2
    }),
    nm_data = "data4",
    nm_series = "births"
  )
  datamods <- list(mod_dth, mod_ins)
  expect_error(
    check_datamods(datamods),
    "no data model for series \"births\""
  )
  datamods <- list(mod_bth, mod_dth, mod_ins, mod_bth2)
  expect_error(
    check_datamods(datamods),
    "two data models for series \"births\""
  )
})


## 'check_df' -----------------------------------------------------------------

test_that("'check_df' returns TRUE with valid input", {
  expect_true(check_df(1))
  expect_true(check_df(0.0001))
  expect_true(check_df(10000))
})

test_that("'check_df' throws correct error with non-numeric", {
  expect_error(
    check_df("1"),
    "'df' has class \"character\""
  )
})

test_that("'check_df' throws correct error with wrong length", {
  expect_error(
    check_df(numeric()),
    "'df' has length 0"
  )
  expect_error(
    check_df(2:3),
    "'df' has length 2"
  )
})

test_that("'check_df' throws correct error with NA", {
  expect_error(
    check_df(NA_real_),
    "'df' is NA"
  )
})

test_that("'check_df' throws correct error with infinite", {
  expect_error(
    check_df(Inf),
    "'df' is infinite"
  )
})

test_that("'check_df' throws correct error with non-positive", {
  expect_error(
    check_df(0),
    "'df' is non-positive"
  )
  expect_error(
    check_df(-1),
    "'df' is non-positive"
  )
})


## 'check_df_classif_nms_complete' --------------------------------------------

test_that("'check_df_classif_nms_complete' returns TRUE with valid inputs - is_popn is FALSE", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 4),
    cohort = 1996:1999,
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_df_classif_nms_complete(df,
    nm_df = "data",
    is_popn = FALSE
  ))
})

test_that("'check_df_classif_nms_complete' returns TRUE with valid inputs - is_popn is TRUE", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 4),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_df_classif_nms_complete(df,
    nm_df = "data",
    is_popn = TRUE
  ))
})

test_that("'check_df_classif_nms_complete' returns correct error with invalid inputs", {
  df <- data.frame(
    age = 0:3,
    cohort = 1996:1999,
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_df_classif_nms_complete(df,
      nm_df = "data",
      is_popn = FALSE
    ),
    "data frame 'data' does not have \"sex\" variable"
  )
})


## 'check_df_data' ------------------------------------------------------------


test_that("'check_df_data' returns TRUE with valid inputs, is_popn = TRUE", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(0, NA, 1, 2)
  )
  expect_true(check_df_data(df,
    nm_df = "data",
    is_popn = TRUE
  ))
})

test_that("'check_df_data' returns TRUE with valid inputs, is_popn = FALSE", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(0, NA, 1, 2)
  )
  df$cohort <- df$time - df$age
  expect_true(check_df_data(df,
    nm_df = "data",
    is_popn = FALSE
  ))
})

test_that("'check_df_data' returns correct error with non-data.frame", {
  expect_error(
    check_df_data(NULL,
      nm_df = "data",
      is_popn = FALSE
    ),
    "problem with 'data' :"
  )
})


## 'check_df_dataconst' -------------------------------------------------------

test_that("'check_df_dataconst' returns TRUE with valid inputs - identical classif vars", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    ratio = c(0, NA, 1.1, 2)
  )
  data <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(1, NA, 2, 3)
  )
  expect_true(check_df_dataconst(df,
    nm_df = "data",
    nm_measure_var = "ratio",
    is_popn = TRUE,
    data = data
  ))
})

test_that("'check_df_dataconst' returns TRUE with valid inputs - diff classif vars", {
  df <- data.frame(
    age = 0:4,
    sex = c(rep(c("Female", "Male"), times = 2), "Female"),
    time = c(rep(2000:2001, each = 2), 2001),
    ratio = c(0, NA, 1.1, 2, 5)
  )
  df <- df[5:1, ]
  data <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(1, NA, 2, 3)
  )
  expect_true(check_df_dataconst(df,
    nm_df = "data",
    nm_measure_var = "ratio",
    is_popn = TRUE,
    data = data
  ))
})

test_that("'check_df_dataconst' returns correct error with non-data.frame", {
  data <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(1, NA, 2, 3)
  )
  expect_error(
    check_df_dataconst(NULL,
      nm_df = "ratio",
      nm_measure_var = "ratio",
      is_popn = FALSE,
      data = data
    ),
    "problem with 'ratio' :"
  )
})

test_that("'check_df_dataconst' returns correct error with reserved word", {
  df <- data.frame(
    age = 0:4,
    sex = c(rep(c("Female", "Male"), times = 2), "Female"),
    time = c(rep(2000:2001, each = 2), 2001),
    count = c(0, NA, 1.1, 2, 5)
  )
  data <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(1, NA, 2, 3)
  )
  expect_error(
    check_df_dataconst(df,
      nm_df = "ratio",
      nm_measure_var = "count",
      is_popn = TRUE,
      data = data
    ),
    "'ratio' has a variable called 'count'"
  )
  df <- data.frame(
    age = 0:4,
    sex = c(rep(c("Female", "Male"), times = 2), "Female"),
    time = c(rep(2000:2001, each = 2), 2001),
    .is_na = c(0, NA, 1.1, 2, 5)
  )
  data <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(1, NA, 2, 3)
  )
  expect_error(
    check_df_dataconst(df,
      nm_df = "ratio",
      nm_measure_var = ".is_na",
      is_popn = TRUE,
      data = data
    ),
    "'ratio' has a variable called '.is_na'"
  )
})

test_that("'check_df_dataconst' returns correct error with missing categories", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    ratio = c(0, NA, 1.1, 2)
  )
  df <- df[-2, ]
  data <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(1, NA, 2, 3)
  )
  expect_error(
    check_df_dataconst(df,
      nm_df = "ratio",
      nm_measure_var = "ratio",
      is_popn = TRUE,
      data = data
    ),
    "'data' has row with values '1', 'Male', '2000', but 'ratio' does not"
  )
})

test_that("'check_df_dataconst' returns correct error with NAs in measure var", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    ratio = c(0, NA, 1.1, 2)
  )
  data <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    count = c(1, 1, 2, 3)
  )
  expect_error(
    check_df_dataconst(df,
      nm_df = "ratio",
      nm_measure_var = "ratio",
      is_popn = TRUE,
      data = data
    ),
    paste(
      "'ratio' variable in 'ratio' is NA but 'count' variable",
      "in 'data' is non-NA for classification variables",
      "'1', 'Male', '2000'"
    )
  )
})


## 'check_df_measure_nm_complete' ---------------------------------------------

test_that("'check_df_measure_nm_complete' returns TRUE with valid inputs", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 4),
    cohort = 1996:1999,
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_df_measure_nm_complete(df,
    nm_df = "data",
    nm_measure_var = "value"
  ))
})

test_that("'check_df_measure_nm_complete' returns correct error with invalid inputs", {
  df <- data.frame(
    age = 0:3,
    cohort = 1996:1999,
    time = rep(2000:2001, each = 2)
  )
  expect_error(
    check_df_measure_nm_complete(df,
      nm_df = "data",
      nm_measure_var = "value"
    ),
    "data frame 'data' does not have \"value\" variable"
  )
})


## 'check_df_nms' -------------------------------------------------------------

test_that("'check_df_nms' returns TRUE with valid inputs - is_popn is FALSE", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 4),
    cohort = 1996:1999,
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_df_nms(df,
    nm_df = "data",
    nm_measure_var = "value",
    is_popn = FALSE
  ))
})

test_that("'check_df_nms' returns TRUE with valid inputs - is_popn is TRUE", {
  df <- data.frame(
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 4),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_df_nms(df,
    nm_df = "data",
    nm_measure_var = "value",
    is_popn = TRUE
  ))
})

test_that("'check_df_nms' returns correct error with invalid inputs", {
  df <- data.frame(
    age = 0:3,
    age = 0:3,
    sex = rep(c("Female", "Male"), times = 4),
    cohort = 1996:1999,
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  names(df)[match("age.1", names(df))] <- "age"
  expect_error(
    check_df_nms(df,
      nm_df = "data",
      nm_measure_var = "value",
      is_popn = FALSE
    ),
    "data frame 'data' has more than one variable called \"age\""
  )
})

test_that("'check_df_nms' returns correct error with invalid inputs", {
  df <- data.frame(
    age = 0:3,
    wrong = 0:3,
    sex = rep(c("Female", "Male"), times = 4),
    cohort = 1996:1999,
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_df_nms(df,
      nm_df = "data",
      nm_measure_var = "value",
      is_popn = FALSE
    ),
    "data frame 'data' has variable called \"wrong\""
  )
})


## 'check_df_sysmod' ----------------------------------------------------------

test_that("'check_df_sysmod' returns error with dataframe containing missing values", {
  df <- data.frame(
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    disp = c(0.1, NA, 1.1, 2)
  )
  expect_error(check_df_sysmod(df,
    nm_df = "disp",
    nm_measure_var = "disp",
    is_births = FALSE,
    min = NULL,
    max = NULL
  ), "problem with 'disp' :")
})


test_that("'check_df_sysmod' returns TRUE with valid inputs - sex and time, not births", {
  df <- data.frame(
    sex = rep(c("Female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    disp = c(0.1, 0, 1.1, 2)
  )
  expect_true(check_df_sysmod(df,
    nm_df = "disp",
    nm_measure_var = "disp",
    is_births = FALSE,
    min = NULL,
    max = NULL
  ))
})

test_that("'check_df_sysmod' returns TRUE with valid inputs - cohort and age, is births", {
  df <- data.frame(
    cohort = 2000:2003,
    age = 10:13,
    disp = c(0.1, 0, 1.1, 2)
  )
  expect_true(check_df_sysmod(df,
    nm_df = "disp",
    nm_measure_var = "disp",
    is_births = TRUE,
    min = NULL,
    max = NULL
  ))
})

test_that("'check_df_sysmod' raises correct error with mean above max", {
  df <- data.frame(
    cohort = 2000:2003,
    age = 0:3,
    mean = c(0.1, 0, 1.1, 2)
  )
  expect_warning(check_df_sysmod(df,
    nm_df = "mean",
    nm_measure_var = "mean",
    is_births = FALSE,
    min = NULL,
    max = 1
  ))
})



## 'check_is_bth_dth' ---------------------------------------------------------

test_that("'check_is_bth_dth' returns TRUE with valid inputs", {
  expect_true(check_is_bth_dth("births"))
  expect_true(check_is_bth_dth("deaths"))
})


test_that("'check_is_bth_dth' returns correct error with invalid inputs", {
  expect_error(
    check_is_bth_dth("population"),
    "'nm_series' is \"population\""
  )
})


## 'check_is_events' ----------------------------------------------------------

test_that("'check_is_events' returns TRUE with valid inputs", {
  expect_true(check_is_events("births"))
  expect_true(check_is_events("deaths"))
  expect_true(check_is_events("ins"))
  expect_true(check_is_events("outs"))
})


test_that("'check_is_events' returns correct error with invalid inputs", {
  expect_error(
    check_is_events("population"),
    "'nm_series' is \"population\""
  )
})


## 'check_is_not_bth_dth' -----------------------------------------------------

test_that("'check_is_not_bth_dth' returns TRUE with valid inputs", {
  expect_true(check_is_not_bth_dth("population"))
  expect_true(check_is_not_bth_dth("ins"))
  expect_true(check_is_not_bth_dth("outs"))
})


test_that("'check_is_not_bth_dth' returns correct error with invalid inputs", {
  expect_error(
    check_is_not_bth_dth("births"),
    "'nm_series' is \"births\""
  )
  expect_error(
    check_is_not_bth_dth("deaths"),
    "'nm_series' is \"deaths\""
  )
})


## 'check_measure_var' --------------------------------------------------------

test_that("'check_measure_var' returns TRUE with valid inputs", {
  df <- data.frame(
    age = rep(0:1, times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_measure_var(df,
    nm_df = "data",
    nm_measure_var = "value",
    min = NULL,
    max = NULL
  ))
})

test_that("'check_measure_var' returns correct error with invalid inputs", {
  df <- data.frame(
    age = rep(0:1, times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, Inf)
  )
  expect_error(
    check_measure_var(df,
      nm_df = "data",
      nm_measure_var = "value",
      min = NULL,
      max = NULL
    ),
    "problem with variable 'value' in data frame 'data' :"
  )
})

test_that("'check_measure_var' returns correct warning with value too low", {
  df <- data.frame(
    age = rep(0:1, times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0.5, NA, 0.0001, 3)
  )
  expect_warning(
    check_measure_var(df,
      nm_df = "data",
      nm_measure_var = "value",
      min = 0.01,
      max = 10
    ),
    paste(
      "minimum value for variable 'value' in data frame 'data' is 1e-04 :",
      "values this low might not be plausible, and might cause numerical problems"
    )
  )
})

test_that("'check_measure_var' returns correct warning with value too high", {
  df <- data.frame(
    age = rep(0:1, times = 2),
    time = rep(2000:2001, each = 2),
    value = c(50, NA, 0.0001, 3)
  )
  expect_warning(
    check_measure_var(df,
      nm_df = "data",
      nm_measure_var = "value",
      min = 0.0000001,
      max = 10
    ),
    paste(
      "maximum value for variable 'value' in data frame 'data' is 50 :",
      "values this high might not be plausible, and might cause numerical problems"
    )
  )
})


## 'check_mods' -----------------------------------------------------------

test_that("'check_mods' returns TRUE with valid inputs", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mod_sys_dth <- sysmod(
    mean = within(classif_vars, {
      mean <- 0.1
    }),
    nm_series = "deaths"
  )
  mod_data_dth <- datamod_exact(
    data = within(classif_vars, {
      count <- 3
    }),
    nm_data = "data2",
    nm_series = "deaths"
  )
  mod_sys_bth <- sysmod(
    mean = within(classif_vars, {
      mean <- 0.1
    }),
    nm_series = "births"
  )
  mod_data_bth <- datamod_exact(
    data = within(classif_vars, {
      count <- 3
    }),
    nm_data = "data3",
    nm_series = "births"
  )
  sysmods <- list(mod_sys_bth, mod_sys_dth)
  datamods <- list(mod_data_bth, mod_data_dth)
  expect_true(check_mods(
    sysmods = sysmods,
    datamods = datamods
  ))
})

test_that("'check_mods' returns correct error with inconsistent deaths classifications", {
  classif_vars1 <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001,
    triangle = 0:1
  )
  classif_vars1$cohort <- with(classif_vars1, time - age - triangle)
  classif_vars1 <- classif_vars1[-match("triangle", names(classif_vars1))]
  classif_vars2 <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2002:2002,
    triangle = 0:1
  )
  classif_vars2$cohort <- with(classif_vars2, time - age - triangle)
  classif_vars2 <- classif_vars2[-match("triangle", names(classif_vars2))]
  mod_sys_dth <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.1
    }),
    nm_series = "deaths"
  )
  mod_data_dth <- datamod_exact(
    data = within(classif_vars2, {
      count <- 3
    }),
    nm_data = "data2",
    nm_series = "deaths"
  )
  mod_sys_bth <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.1
    }),
    nm_series = "births"
  )
  mod_data_bth <- datamod_exact(
    data = within(classif_vars2, {
      count <- 3
    }),
    nm_data = "data3",
    nm_series = "births"
  )
  sysmods <- list(mod_sys_bth, mod_sys_dth)
  datamods <- list(mod_data_bth, mod_data_dth)
  expect_error(
    check_mods(
      sysmods = sysmods,
      datamods = datamods
    ),
    paste(
      "variable 'cohort' in system model for deaths",
      "has different categories from variable 'cohort'",
      "in data model for deaths"
    )
  )
})

test_that("'check_mods' returns correct error when sysmod for births missing age group in data", {
  classif_vars <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001,
    triangle = 0:1
  )
  classif_vars$cohort <- with(classif_vars, time - age - triangle)
  classif_vars <- classif_vars[-match("triangle", names(classif_vars))]
  mod_sys_dth <- sysmod(
    mean = within(classif_vars, {
      mean <- 0.1
    }),
    nm_series = "deaths"
  )
  mod_data_dth <- datamod_exact(
    data = within(classif_vars, {
      count <- 3
    }),
    nm_data = "data2",
    nm_series = "deaths"
  )
  mod_sys_bth <- sysmod(
    mean = subset(
      within(classif_vars, {
        mean <- 0.1
      }),
      age > 0
    ),
    nm_series = "births"
  )
  mod_data_bth <- datamod_exact(
    data = within(classif_vars, {
      count <- 3
    }),
    nm_data = "data3",
    nm_series = "births"
  )
  sysmods <- list(mod_sys_bth, mod_sys_dth)
  datamods <- list(mod_data_bth, mod_data_dth)
  expect_error(
    check_mods(
      sysmods = sysmods,
      datamods = datamods
    ),
    "data model for births has data for age 0, but system model for births does not have rate for age 0"
  )
})


## 'check_nm_data_clash' ------------------------------------------------------

test_that("'check_nm_data_clash' returns TRUE with valid inputs", {
  expect_true(check_nm_data_clash("mydata"))
})

test_that("'check_nm_data_clash' returns correct error with invalid inputs", {
  expect_error(
    check_nm_data_clash("deaths"),
    paste(
      "dataset has same name \\[\"deaths\"\\] as a demographic series :",
      "demographic series are \"population\", \"births\",",
      "\"deaths\", \"ins\", \"outs\""
    )
  )
})


## 'check_no_na' --------------------------------------------------------------

test_that("'check_no_na' returns TRUE with valid inputs", {
  df <- data.frame(x = 1:3)
  expect_true(check_no_na(df = df, nm_x = "x", nm_df = "data"))
})

test_that("'check_no_na' throws correct error with NA", {
  df <- data.frame(x = c(1:3, NA))
  expect_error(
    check_no_na(df = df, nm_x = "x", nm_df = "data"),
    "variable 'x' in data frame 'data' has NAs"
  )
})


## 'check_scale' --------------------------------------------------------------

test_that("'check_scale' returns TRUE with valid inputs", {
  expect_true(check_scale(1L, x_arg = "x", zero_ok = FALSE))
  expect_true(check_scale(0.001, x_arg = "x", zero_ok = FALSE))
})

test_that("'check_scale' returns correct error with non-numeric", {
  expect_error(
    check_scale("1", x_arg = "x", zero_ok = FALSE),
    "'x' has class \"character\""
  )
})

test_that("'check_scale' returns correct error with wrong length", {
  expect_error(
    check_scale(1:2, x_arg = "x", zero_ok = FALSE),
    "'x' has length 2"
  )
})

test_that("'check_scale' returns correct error with NA", {
  expect_error(
    check_scale(NA_real_, x_arg = "x", zero_ok = FALSE),
    "'x' is NA"
  )
})

test_that("'check_scale' returns correct error with Inf", {
  expect_error(
    check_scale(Inf, x_arg = "x", zero_ok = FALSE),
    "'x' is infinite"
  )
})

test_that("'check_scale' returns correct error with negative", {
  expect_true(check_scale(0, x_arg = "x", zero_ok = TRUE))
  expect_error(
    check_scale(-1, x_arg = "x", zero_ok = TRUE),
    "'x' is negative"
  )
})

test_that("'check_scale' returns correct error with non-positive", {
  expect_error(
    check_scale(0, x_arg = "x", zero_ok = FALSE),
    "'x' is non-positive"
  )
  expect_error(
    check_scale(-1, x_arg = "x", zero_ok = FALSE),
    "'x' is non-positive"
  )
})


## 'check_sex' ----------------------------------------------------------------

test_that("'check_sex' returns TRUE with valid inputs", {
  df <- data.frame(
    sex = rep(c("Female", "Male"), times = 4),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_sex(df, nm_df = "data"))
})

test_that("'check_sex' returns correct error with invalid inputs (0/1 coding)", {
  df <- data.frame(
    sex = rep(0:1, times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_sex(df, nm_df = "data"),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})

test_that("'check_sex' returns correct error with invalid inputs (female/male coding)", {
  df <- data.frame(
    sex = rep(c("female", "male"), times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_sex(df, nm_df = "data"),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})

test_that("'check_sex' returns correct error with invalid inputs (Female/male coding)", {
  df <- data.frame(
    sex = rep(c("Female", "male"), times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_sex(df, nm_df = "data"),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})

test_that("'check_sex' returns correct error with invalid inputs (female/Male coding)", {
  df <- data.frame(
    sex = rep(c("female", "Male"), times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_sex(df, nm_df = "data"),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})

test_that("'check_sex' returns correct error with invalid inputs (Female/Male/Other coding)", {
  df <- data.frame(
    sex = rep(c("Female", "Male", "Other"), times = 2),
    time = rep(2000:2001, each = 3),
    value = c(0, NA, 1, 0.2, 0.1, 0.5)
  )
  expect_error(
    check_sex(df, nm_df = "data"),
    "problem with invalid coding for 'sex' in data frame 'data':"
  )
})



## 'check_sex_complete' -------------------------------------------------------

test_that("'check_sex_complete' returns TRUE with valid inputs", {
  df <- data.frame(
    sex = rep(c("Female", "Male"), times = 4),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_sex_complete(df, nm_df = "data"))
})

test_that("'check_sex_complete' returns correct error with invalid inputs", {
  df <- data.frame(
    sex = rep(c("F", "M"), times = 2),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_sex_complete(df, nm_df = "data"),
    "'sex' variable in data frame 'data' does not have category \"Female\""
  )
})


## 'check_sysmods' ------------------------------------------------------------

test_that("'check_sysmods' returns TRUE with valid inputs", {
  classif_vars1 <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001
  )
  classif_vars2 <- data.frame(age = 0:2)
  classif_vars3 <- data.frame(age = 1)
  mod_bth <- sysmod(
    mean = within(classif_vars3, {
      mean <- 0.2
    }),
    disp = 0.2,
    nm_series = "births"
  )
  mod_dth <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.2
    }),
    disp = 0.2,
    nm_series = "deaths"
  )
  mod_ins <- sysmod(
    mean = within(classif_vars1, {
      mean <- 1.2
    }),
    nm_series = "ins"
  )
  mod_outs <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.1
    }),
    nm_series = "outs"
  )
  sysmods <- list(mod_bth, mod_dth, mod_ins, mod_outs)
  expect_true(check_sysmods(sysmods))
})

test_that("'check_sysmods' returns current error with duplicate 'nm_series'", {
  classif_vars1 <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001
  )
  classif_vars2 <- data.frame(age = 0:2)
  classif_vars3 <- data.frame(age = 1)
  mod_bth <- sysmod(
    mean = within(classif_vars3, {
      mean <- 0.2
    }),
    disp = 0.2,
    nm_series = "births"
  )
  mod_dth <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.2
    }),
    disp = 0.2,
    nm_series = "deaths"
  )
  mod_ins <- sysmod(
    mean = within(classif_vars1, {
      mean <- 1.2
    }),
    nm_series = "ins"
  )
  mod_bth2 <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.1
    }),
    nm_series = "births"
  )
  sysmods <- list(mod_bth, mod_dth, mod_ins, mod_bth2)
  expect_error(
    check_sysmods(sysmods),
    "two system models have the same value for 'nm_series' \\[\"births\"\\]"
  )
})

test_that("'check_sysmods' returns current error with inconsistent variables - age", {
  classif_vars1 <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2000:2001
  )
  classif_vars2 <- data.frame(age = 0:3)
  mod_bth <- sysmod(
    mean = within(classif_vars2, {
      mean <- 0.2
    }),
    disp = 0.2,
    nm_series = "births"
  )
  mod_dth <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.2
    }),
    disp = 0.2,
    nm_series = "deaths"
  )
  mod_ins <- sysmod(
    mean = within(classif_vars2, {
      mean <- 1.2
    }),
    nm_series = "ins"
  )
  mod_outs <- sysmod(
    mean = within(classif_vars1, {
      mean <- 0.1
    }),
    nm_series = "outs"
  )
  sysmods <- list(mod_bth, mod_dth, mod_ins, mod_outs)
  expect_error(
    check_sysmods(sysmods),
    "system models have inconsistent categories for variable 'age'"
  )
})


## 'check_time' ---------------------------------------------------------------

test_that("'check_time' returns TRUE with valid inputs", {
  df <- data.frame(
    sex = rep(c("Female", "Male"), times = 4),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_time(df, nm_df = "data"))
})

test_that("'check_time' returns correct error with invalid inputs", {
  df <- data.frame(
    sex = rep(c("Female", "Male"), times = 2),
    time = c(0.1, 2000, 2000, 2000),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_time(df, nm_df = "data"),
    "problem with variable 'time' in data frame 'data' :"
  )
})


## 'check_time_complete' ------------------------------------------------------

test_that("'check_time_complete' returns TRUE with valid inputs", {
  df <- data.frame(
    sex = rep(c("Female", "Male"), times = 4),
    time = rep(2000:2001, each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_true(check_time_complete(df, nm_df = "data"))
})

test_that("'check_time_complete' returns correct error with invalid inputs", {
  df <- data.frame(
    sex = rep(c("F", "M"), times = 2),
    time = rep(c(2000, 2002), each = 2),
    value = c(0, NA, 1, 0.2)
  )
  expect_error(
    check_time_complete(df, nm_df = "data"),
    "'time' variable in data frame 'data' has gaps : missing value '2001'"
  )
})
