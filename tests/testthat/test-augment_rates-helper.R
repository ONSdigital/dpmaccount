## 'get_best_value_one' -------------------------------------------------------

test_that("'get_best_value_one' works with valid inputs", {
  df <- expand.grid(
    age = 0:1, sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  df$val1 <- c(1:2, NA, NA)
  df$val2 <- c(11:13, NA)
  df$val3 <- 21:24
  ans_obtained <- get_best_value_one(df, nm = "val")
  ans_expected <- cbind(df[1:2], val = c(1L, 2L, 13L, 24L))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_exposure_direct' -----------------------------------------------------

test_that("'make_exposure_direct' works with age, no cohort", {
  popn_df <- expand.grid(
    time = 2001:2003,
    age = 10:12,
    sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  popn_df$population1 <- seq_len(nrow(popn_df))
  popn_df$population1[5] <- NA
  popn_df$population2 <- 100
  events_df <- expand.grid(
    time = 2002:2003,
    age = 10:12,
    sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  ans_obtained <- make_exposure_direct(
    popn_df = popn_df,
    events_df = events_df
  )
  expect_true(!anyNA(ans_obtained))
  expect_identical(length(ans_obtained), nrow(events_df))
  expect_identical(ans_obtained[1:2], c(1.5, 2.5))
  popn_perm <- popn_df[seq.int(from = nrow(popn_df), to = 1), ]
  ans_perm <- make_exposure_direct(
    popn_df = popn_perm,
    events_df = events_df
  )
  expect_identical(ans_perm, ans_obtained)
})

test_that("'make_exposure_direct' works with cohort, no age", {
  popn_df <- expand.grid(
    time = 2001:2003,
    cohort = 1990:1992,
    sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  popn_df$population1 <- seq_len(nrow(popn_df))
  popn_df$population1[5] <- NA
  popn_df$population2 <- 100
  events_df <- expand.grid(
    time = 2002:2003,
    cohort = 1990:1992,
    sex = c("Female", "Male"),
    tri = 0:1,
    KEEP.OUT.ATTRS = FALSE
  )
  events_df$age <- with(events_df, time - cohort - tri)
  events_df <- events_df[-4]
  ans_obtained <- make_exposure_direct(
    popn_df = popn_df,
    events_df = events_df
  )
  expect_true(!anyNA(ans_obtained))
  expect_identical(length(ans_obtained), nrow(events_df))
  expect_identical(ans_obtained[1:2], c(1.5, 2.5))
  popn_perm <- popn_df[seq.int(from = nrow(popn_df), to = 1), ]
  ans_perm <- make_exposure_direct(
    popn_df = popn_perm,
    events_df = events_df
  )
  expect_identical(ans_perm, ans_obtained)
})

test_that("'make_exposure_direct' works with age, with cohort", {
  popn_df <- expand.grid(
    time = 2001:2003,
    age = 11:13,
    sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  popn_df$cohort <- with(popn_df, time - age)
  popn_df$population1 <- seq_len(nrow(popn_df))
  popn_df$population1[5] <- NA
  popn_df$population2 <- 100
  events_df <- expand.grid(
    time = 2002:2003,
    age = 11:13,
    sex = c("Female", "Male"),
    tri = 0:1,
    KEEP.OUT.ATTRS = FALSE
  )
  events_df$cohort <- with(events_df, time - age - tri)
  events_df <- events_df[-4]
  ans_obtained <- make_exposure_direct(
    popn_df = popn_df,
    events_df = events_df
  )
  expect_true(!anyNA(ans_obtained))
  expect_identical(length(ans_obtained), nrow(events_df))
  expect_identical(ans_obtained[1:2], c(0.75, 1.25))
  popn_perm <- popn_df[seq.int(from = nrow(popn_df), to = 1), ]
  ans_perm <- make_exposure_direct(
    popn_df = popn_perm,
    events_df = events_df
  )
  expect_identical(ans_perm, ans_obtained)
})

test_that("'make_exposure_direct' works no age, no cohort", {
  popn_df <- expand.grid(
    time = 2001:2003,
    sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  popn_df$population1 <- seq_len(nrow(popn_df))
  popn_df$population1[5] <- NA
  popn_df$population2 <- 100
  events_df <- expand.grid(
    time = 2002:2003,
    sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  ans_obtained <- make_exposure_direct(
    popn_df = popn_df,
    events_df = events_df
  )
  expect_true(!anyNA(ans_obtained))
  expect_identical(length(ans_obtained), nrow(events_df))
  expect_identical(ans_obtained, c(1.5, 2.5, 52, 53))
  popn_perm <- popn_df[seq.int(from = nrow(popn_df), to = 1), ]
  ans_perm <- make_exposure_direct(
    popn_df = popn_perm,
    events_df = events_df
  )
  expect_identical(ans_perm, ans_obtained)
})


## 'make_exposure_direct_inner' -----------------------------------------------

test_that("'make_exposure_direct_inner' ordering of datasets works", {
  # sort by time
  df1 <- data.frame(
    time = 2000:2005,
    age = 10,
    cohort = 1990:1995,
    sex = "Female",
    population = 21:26
  )[c(3, 1, 5, 2, 4, 6), ]
  # sort by age
  df2 <- data.frame(
    age = 10,
    cohort = 1990:1995,
    sex = "Female",
    population = 21:26
  )[c(3, 1, 5, 2, 4, 6), ]
  # sort by cohort
  df3 <- data.frame(
    cohort = 1990:1995,
    sex = "Female",
    population = 21:26
  )[c(3, 1, 5, 2, 4, 6), ]

  ans_obtained1 <- make_exposure_direct_inner(df1, has_lexis = TRUE)
  ans_obtained2 <- make_exposure_direct_inner(df2, has_lexis = TRUE)
  ans_obtained3 <- make_exposure_direct_inner(df3, has_lexis = TRUE)

  ans_expected1 <- data.frame(
    time = 2001:2005,
    age = 10,
    cohort = 1991:1995,
    sex = "Female",
    exposure = 0.5 * c(21.5, 22.5, 23.5, 24.5, 25.5)
  )

  ans_expected2 <- df2[, -4]
  ans_expected2 <- ans_expected2[-1, ]
  ans_expected2$exposure <- 0.5 * c(22, 23, 23.5, 23, 25)


  ans_expected3 <- data.frame(
    cohort = 1991:1995,
    sex = "Female",
    exposure = 0.5 * c(21.5, 22.5, 23.5, 24.5, 25.5)
  )

  rownames(ans_expected1) <- NULL
  rownames(ans_expected2) <- NULL
  rownames(ans_expected3) <- NULL

  expect_identical(ans_obtained1, ans_expected1)
  expect_identical(ans_obtained2, ans_expected2)
  expect_identical(ans_obtained3, ans_expected3)
})

test_that("'make_exposure_direct_inner' works with no triangles", {
  set.seed(0)
  df <- data.frame(
    time = 2000:2005,
    age = 10,
    sex = "Female",
    population = 21:26
  )[c(3, 1, 5, 2, 4, 6), ]
  ans_obtained <- make_exposure_direct_inner(df, has_lexis = FALSE)
  ans_expected <- data.frame(
    time = 2001:2005,
    age = 10,
    sex = "Female",
    exposure = c(21.5, 22.5, 23.5, 24.5, 25.5)
  )
  rownames(ans_expected) <- NULL
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_exposure_direct_inner' works with triangles", {
  set.seed(0)
  df <- data.frame(
    time = 2000:2005,
    age = 10,
    cohort = 1990:1995,
    sex = "Female",
    population = 21:26
  )[c(3, 1, 5, 2, 4, 6), ]
  ans_obtained <- make_exposure_direct_inner(df, has_lexis = TRUE)
  ans_expected <- data.frame(
    time = 2001:2005,
    age = 10,
    cohort = 1991:1995,
    sex = "Female",
    exposure = 0.5 * c(21.5, 22.5, 23.5, 24.5, 25.5)
  )
  rownames(ans_expected) <- NULL
  expect_identical(ans_obtained, ans_expected)
})


## 'unpivot_sex_index_births' -------------------------------------------------

test_that("'unpivot_sex_index_births' throws error if passed data frame without sex variable", {
  df <- expand.grid(age = 0:2, gender = c("Female", "Male"), time = 2001:2005)
  df$births <- ifelse(df$age > 0, 1, NA)

  expect_error(
    unpivot_sex_index_births(df = df, nm_bth = "births"),
    "data frame does not contain 'sex' variable"
  )
})

test_that("'unpivot_sex_index_births' works with valid inputs", {
  df <- expand.grid(age = 0:2, sex = c("Female", "Male"), time = 2001:2005)
  df$births <- ifelse(df$age > 0, 1, NA)
  ans_obtained <- unpivot_sex_index_births(df = df, nm_bth = "births")
  ans_expected <- expand.grid(
    age = 0:2,
    sex = c("Female", "Male"),
    time = 2001:2005
  )
  ans_expected$births <- ifelse(df$age > 0 & df$sex == "Female", 2, NA)
  expect_identical(ans_obtained, ans_expected)
})
