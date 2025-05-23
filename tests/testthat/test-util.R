## 'draw_counts' --------------------------------------------------------

test_that("'draw_counts works with 'stk_init' uncertain - adj needed", {
  set.seed(0)
  n_draw <- 100L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K + 1, by = 10))
  sd <- diag(seq(from = 0.3, length.out = 2 * K + 1, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  val_dth <- rep(9, times = K)
  ans <- draw_counts(
    n_draw = n_draw,
    mean = mean,
    var = var,
    val_dth = val_dth,
    cohort = 2000,
    sex = "F"
  )
  expect_equal(
    ans$val_stk[1, ] - ans$val_stk_init,
    ans$val_ins[1, ] - ans$val_dth[1, ] - ans$val_outs[1, ]
  )
  expect_equal(
    ans$val_stk[3, ] - ans$val_stk[2, ],
    ans$val_ins[3, ] - ans$val_dth[3, ] - ans$val_outs[3, ]
  )
  expect_true(all(unlist(ans) >= 0))
  expect_identical(ncol(ans$val_stk), n_draw)
  expect_true(ans$n_adj > 0L)
})


## 'elements_setequal' --------------------------------------------------------

test_that("'elements_setequal' works with valid inputs", {
  expect_true(elements_setequal(list()))
  expect_true(elements_setequal(list(1:3)))
  expect_true(elements_setequal(list(1:3, 3:1)))
  expect_false(elements_setequal(list(1:3, 3:0)))
  expect_true(elements_setequal(list(1:3, 3:1, c(1L, 1L, 3L, 2L, 3L))))
  expect_false(elements_setequal(list(1:3, 1:3, 3:0)))
  expect_false(elements_setequal(list(1:3, c(NA, 1:3))))
})


## 'full_join_time' --------------------------------------------------------


test_that("'full_join_age' works with valid inputs", {
  df1 <- data.frame(
    time = 2000:2003,
    data1 = 1:4
  )
  df2 <- data.frame(
    time = 2001:2004,
    data2 = 11:14
  )
  ans_obtained <- full_join_time(x = df1, y = df2)
  ans_expected <- data.frame(
    time = 2000:2004,
    data1 = c(1:4, NA),
    data2 = c(NA, 11:14)
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'full_join_timeage' --------------------------------------------------------

test_that("'full_join_agetime' works with valid inputs", {
  df1 <- data.frame(
    age = rep(1:2, times = 2),
    time = rep(2000:2001, each = 2),
    data1 = 1:4
  )
  df2 <- data.frame(
    age = rep(0:1, times = 2),
    time = rep(2000:2001, each = 2),
    data2 = 11:14
  )
  ans_obtained <- full_join_timeage(x = df1, y = df2)
  ans_expected <- data.frame(
    time = rep(2000:2001, each = 3),
    age = rep(0:2, times = 2),
    data1 = c(NA, 1:2, NA, 3:4),
    data2 = c(11:12, NA, 13:14, NA)
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'matrix_to_list_cols' ------------------------------------------------------

test_that("'matrix_to_list_cols' works with valid inputs", {
  m <- matrix(1:6, nr = 2, dimnames = list(x = 1:2, b = 1:3))
  ans_obtained <- matrix_to_list_cols(m)
  ans_expected <- list(1:2, 3:4, 5:6)
  expect_identical(ans_obtained, ans_expected)
})


## 'nest_to_df' ---------------------------------------------------------------

test_that("'nest_to_df' works with valid inputs", {
  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }
  df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2001:2005)
  df$count <- seq_len(nrow(df))
  ans <- nest_to_df(df, nms_data = "count", nms_group = c("sex", "age"), nm_value = "val")
  expect_identical(names(ans), c("sex", "age", "val"))
  expect_identical(
    unname(sapply(ans, class)),
    c("character", "integer", "list")
  )
  expect_identical(
    ans$val[[1L]],
    unrowname(df[df$age == 0 & df$sex == "F", "count", drop = FALSE])
  )
})


## 'split_to_df' --------------------------------------------------------------

test_that("'split_to_df' works with age and sex as grouping variables - data frames", {
  df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2001:2005)
  df$count <- seq_len(nrow(df))
  l <- split(df[c("time", "count")], df[c("age", "sex")])
  ans <- split_to_df(l, nms_group = c("age", "sex"), nm_value = "val")
  expect_identical(names(ans), c("age", "sex", "val"))
  expect_identical(
    unname(sapply(ans, class)),
    c("integer", "character", "list")
  )
  expect_identical(
    ans$val[[1L]],
    df[df$age == 0 & df$sex == "F", c("time", "count")]
  )
})

test_that("'split_to_df' works with time and cohort as grouping variables - data frames", {
  df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2001:2005, triangle = 0:1)
  df$cohort <- with(df, time - age - triangle)
  df$count <- seq_len(nrow(df))
  l <- split(df[c("age", "sex", "count")], df[c("cohort", "time")])
  ans <- split_to_df(l, nms_group = c("cohort", "time"), nm_value = "val")
  expect_identical(names(ans), c("cohort", "time", "val"))
  expect_identical(
    unname(sapply(ans, class)),
    c("integer", "integer", "list")
  )
  expect_identical(
    ans$val[[2L]],
    df[df$cohort == 1999 & df$time == 2001, c("age", "sex", "count")]
  )
})

test_that("'split_to_df' works with sex as grouping variables - data frames", {
  df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2001:2005)
  df$count <- seq_len(nrow(df))
  l <- split(df[c("time", "count")], df["sex"])
  ans <- split_to_df(l, nms_group = "sex", nm_value = "val")
  expect_identical(names(ans), c("sex", "val"))
  expect_identical(
    unname(sapply(ans, class)),
    c("character", "list")
  )
  expect_identical(
    ans$val[[1L]],
    df[df$sex == "F", c("time", "count")]
  )
})

test_that("'split_to_df' works with age and sex as grouping variables - list", {
  df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2001:2005)
  df$count <- seq_len(nrow(df))
  l <- split(df[["count"]], df[c("age", "sex")])
  ans <- split_to_df(l, nms_group = c("age", "sex"), nm_value = "val")
  expect_identical(names(ans), c("age", "sex", "val"))
  expect_identical(
    unname(sapply(ans, class)),
    c("integer", "character", "list")
  )
  expect_identical(
    ans$val[[1L]],
    df$count[df$age == 0 & df$sex == "F"]
  )
})


## 'summarise_codatamods' -----------------------------------------------------

test_that("'summarise_codatamods' works with valid inputs", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_sd = rep(0.1, 10)
  )
  x1 <- new_codatamod_norm(args)
  x2 <- new_codatamod_norm(args)
  x2$data <- as.double(101:110)
  mods <- list(df1 = x1, df2 = x2)
  ans_obtained <- summarise_codatamods(mods)
  ans_expected <- data.frame(
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    age = rep(0:4, each = 2),
    df1 = as.double(1:10),
    df2 = as.double(101:110)
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'summarise_estimated_means' ------------------------------------------------

test_that("'summarise_estimated_means' works with valid inputs - initial stock estimated", {
  mean <- c(
    log_val_stk_init = 4, log_val_ins = -1, log_val_ins = -2,
    log_val_outs = 1, log_val_outs = 2
  )
  prior_stk_init <- list(mean = 3, sd = Inf)
  val_dth <- c(1, 1)
  ans_obtained <- summarise_estimated_means(mean = mean, prior_stk_init = prior_stk_init, dths = val_dth)
  ans_expected <- data.frame(
    mean_stk_end = c(
      exp(4) + exp(-1) - exp(1) - 1,
      exp(4) + exp(-1) + exp(-2) -
        exp(1) - exp(2) - 1 - 1
    ),
    mean_ins = exp(c(-1, -2)),
    mean_outs = exp(c(1, 2))
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'summarise_estimated_means' works with valid inputs - initial stock fixed", {
  mean <- c(
    log_val_ins = -1, log_val_ins = -2,
    log_val_outs = 1, log_val_outs = 2
  )
  prior_stk_init <- list(mean = 10, sd = 0)
  val_dth <- c(1, 1)
  ans_obtained <- summarise_estimated_means(mean = mean, prior_stk_init = prior_stk_init, dths = val_dth)
  ans_expected <- data.frame(
    mean_stk_end = c(
      10 + exp(-1) - exp(1) - 1,
      10 + exp(-1) + exp(-2) -
        exp(1) - exp(2) - 1 - 1
    ),
    mean_ins = exp(c(-1, -2)),
    mean_outs = exp(c(1, 2))
  )
  expect_equal(ans_obtained, ans_expected)
})


## 'summarise_sysmods' --------------------------------------------------------

test_that("'summarise_sysmods' works with valid inputs", {
  mod1 <- data.frame(
    mean = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L)
  )
  mod2 <- data.frame(
    mean = as.double(101:110),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L)
  )
  mods <- list(df1 = mod1, df2 = mod2)
  ans_obtained <- summarise_sysmods(mods)
  ans_expected <- data.frame(
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    age = rep(0:4, each = 2),
    df1 = as.double(1:10),
    df2 = as.double(101:110)
  )
  expect_identical(ans_obtained, ans_expected)
})

## 'make_seed_account' ---------------------------------------------------------

test_that("'make_seed_account' works with valid inputs", {
  seed_list <- make_seed_account(seed_in = 0)
  expect_identical(names(seed_list), c(
    "prior_rate_seed", "draw_counts_seed",
    "draw_par_datamods_seed", "draw_rates_ins_seed",
    "draw_rates_outs_seed", "draw_rates_dth_seed",
    "draw_rates_bth_seed", "post_pred_pop_seed", "post_pred_events_seed"
  ))
})

test_that("'make_seed_account' works with default inputs", {
  seed_list <- make_seed_account()
  expect_identical(names(seed_list), c(
    "prior_rate_seed", "draw_counts_seed",
    "draw_par_datamods_seed", "draw_rates_ins_seed",
    "draw_rates_outs_seed", "draw_rates_dth_seed",
    "draw_rates_bth_seed", "post_pred_pop_seed", "post_pred_events_seed"
  ))
})

test_that("'make_seed_account' returns expected seeds - seed_in = 0", {
  seed_list <- make_seed_account(seed_in = 0)
  seed_list_expected <- list(
    prior_rate_seed = 1703756793L,
    draw_counts_seed = 1598263975L,
    draw_par_datamods_seed = 1753232290L,
    draw_rates_ins_seed = 1711075799L,
    draw_rates_outs_seed = 690659598L,
    draw_rates_dth_seed = 265368763L,
    draw_rates_bth_seed = 758296545L,
    post_pred_pop_seed = 1649722645L,
    post_pred_events_seed = 2137634742L
  )
  expect_identical(seed_list, seed_list_expected)
})
