## 'add_summaries' ------------------------------------------------------------

test_that("'add_summaries' works with valid inputs", {
  set.seed(0)
  df <- expand.grid(age = 0:1, sex = c("Female", "Male"), KEEP.OUT.ATTRS = FALSE)
  df$val <- list(rnorm(100), rnorm(100), rnorm(100), rnorm(100))
  ans_obtained <- add_summaries(df, vname = "val", width = 0.95)
  ans_expected <- data.frame(df,
    val.fitted = sapply(df$val, mean),
    val.lower = sapply(df$val, quantile, prob = 0.025),
    val.upper = sapply(df$val, quantile, prob = 0.975)
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'add_summaries' thows correct error with wrong column name", {
  set.seed(0)
  df <- expand.grid(age = 0:1, sex = c("Female", "Male"), KEEP.OUT.ATTRS = FALSE)
  df$val <- list(rnorm(100), rnorm(100), rnorm(100), rnorm(100))
  expect_error(
    add_summaries(df, vname = "wrong", width = 0.9),
    "data frame does not contain a variable called 'wrong'"
  )
})

test_that("'add_summaries' thows correct error with non-list column", {
  set.seed(0)
  df <- expand.grid(age = 0:1, sex = c("Female", "Male"), KEEP.OUT.ATTRS = FALSE)
  df$val <- rnorm(4)
  expect_error(
    add_summaries(df, vname = "val", width = 0.9),
    "'val' is not a list column"
  )
})


## 'adjust_counts' ------------------------------------------------------------

test_that("'adjust_counts' works with is_known_stk_init = TRUE, val_stk_init nonzero", {
  val_stk_init <- 2
  val_dth <- c(0, 2, 1, 0)
  val_ins <- c(0.3, 3.2, 5.3, 5.6)
  val_outs <- c(2.1, 13, 8.3, 3)
  ans <- adjust_counts(
    val_stk_init = val_stk_init,
    val_dth = val_dth,
    val_ins = val_ins,
    val_outs = val_outs,
    is_known_stk_init = TRUE
  )
  expect_true(all(unlist(ans) >= 0))
})

test_that("'adjust_counts' works with is_known_stk_init = TRUE, val_stk_init zero", {
  val_stk_init <- 0
  val_dth <- c(0, 2, 1, 0)
  val_ins <- c(0.3, 3.2, 5.3, 5.6)
  val_outs <- c(2.1, 13, 8.3, 3)
  ans <- adjust_counts(
    val_stk_init = val_stk_init,
    val_dth = val_dth,
    val_ins = val_ins,
    val_outs = val_outs,
    is_known_stk_init = TRUE
  )
  expect_true(all(unlist(ans) >= 0))
})

test_that("'adjust_counts' works with is_known_stk_init = TRUE, all near 0", {
  val_stk_init <- 0
  val_dth <- rep(0, 10)
  val_ins <- rep(1e-6, 10)
  val_outs <- rep(1e-6, 10)
  ans <- adjust_counts(
    val_stk_init = val_stk_init,
    val_dth = val_dth,
    val_ins = val_ins,
    val_outs = val_outs,
    is_known_stk_init = TRUE
  )
  expect_true(all(unlist(ans) >= 0))
})

test_that("'adjust_counts' works with is_known_stk_init = FALSE, val_stk_init nonzero", {
  val_stk_init <- 2
  val_dth <- c(0, 2, 1, 0)
  val_ins <- c(0.3, 3.2, 5.3, 5.6)
  val_outs <- c(2.1, 13, 8.3, 3)
  ans <- adjust_counts(
    val_stk_init = val_stk_init,
    val_dth = val_dth,
    val_ins = val_ins,
    val_outs = val_outs,
    is_known_stk_init = FALSE
  )
  expect_true(all(unlist(ans) >= 0))
})

test_that("'adjust_counts' works with is_known_stk_init = FALSE, val_stk_init near zero", {
  val_stk_init <- 1e-10
  val_dth <- c(0, 2, 1, 0)
  val_ins <- c(0.3, 3.2, 5.3, 5.6)
  val_outs <- c(2.1, 13, 8.3, 3)
  ans <- adjust_counts(
    val_stk_init = val_stk_init,
    val_dth = val_dth,
    val_ins = val_ins,
    val_outs = val_outs,
    is_known_stk_init = FALSE
  )
  expect_true(all(unlist(ans) >= 0))
})

test_that("'adjust_counts' works with is_known_stk_init = FALSE, all near 0", {
  val_stk_init <- 1e-10
  val_dth <- rep(0, 10)
  val_ins <- rep(1e-6, 10)
  val_outs <- rep(1e-6, 10)
  ans <- adjust_counts(
    val_stk_init = val_stk_init,
    val_dth = val_dth,
    val_ins = val_ins,
    val_outs = val_outs,
    is_known_stk_init = FALSE
  )
  expect_true(all(unlist(ans) >= 0))
})


## 'aggregate_count' ----------------------------------------------------------

test_that("'aggregate_count' works with valid inputs - one variable, na_rm is TRUE", {
  set.seed(0)
  df <- expand.grid(
    age = 0:1,
    sex = c("Female", "Male"),
    time = 2000:2001,
    KEEP.OUT.ATTRS = FALSE
  )
  df$num <- rnorm(8)
  df$num[8] <- NA
  df$ls <- replicate(n = 8, rnorm(5), simplify = FALSE)
  df$ls[[8]][5] <- NA
  ans_obtained <- aggregate_count(df, collapse = "age", na_rm = TRUE)
  df_adj <- df
  df_adj$num[8] <- 0
  df_adj$ls[[8]][5] <- 0
  ans_expected <- tibble::tibble(unique(df_adj[c("sex", "time")]),
    num = c(
      df_adj$num[1] + df_adj$num[2],
      df_adj$num[3] + df_adj$num[4],
      df_adj$num[5] + df_adj$num[6],
      df_adj$num[7] + df_adj$num[8]
    ),
    ls = list(
      df_adj$ls[[1]] + df_adj$ls[[2]],
      df_adj$ls[[3]] + df_adj$ls[[4]],
      df_adj$ls[[5]] + df_adj$ls[[6]],
      df_adj$ls[[7]] + df_adj$ls[[8]]
    )
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'aggregate_count' works with valid inputs - one variable, na_rm is FALSE", {
  set.seed(0)
  df <- expand.grid(
    age = 0:1,
    sex = c("Female", "Male"),
    time = 2000:2001,
    KEEP.OUT.ATTRS = FALSE
  )
  df$num <- rnorm(8)
  df$num[8] <- NA
  df$ls <- replicate(n = 8, rnorm(5), simplify = FALSE)
  df$ls[[8]][5] <- NA
  ans_obtained <- aggregate_count(df, collapse = "age", na_rm = FALSE)
  ans_expected <- tibble::tibble(unique(df[c("sex", "time")]),
    num = c(
      df$num[1] + df$num[2],
      df$num[3] + df$num[4],
      df$num[5] + df$num[6],
      df$num[7] + df$num[8]
    ),
    ls = list(
      df$ls[[1]] + df$ls[[2]],
      df$ls[[3]] + df$ls[[4]],
      df$ls[[5]] + df$ls[[6]],
      df$ls[[7]] + df$ls[[8]]
    )
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'aggregate_count' works with valid inputs - two variables, na_rm is TRUE", {
  set.seed(0)
  df <- expand.grid(
    age = 0:1,
    sex = c("Female", "Male"),
    time = 2000:2001,
    KEEP.OUT.ATTRS = FALSE
  )
  df$num <- rnorm(8)
  df$num[8] <- NA
  df$ls <- replicate(n = 8, rnorm(5), simplify = FALSE)
  df$ls[[8]][5] <- NA
  ans_obtained <- aggregate_count(df, collapse = c("age", "sex"), na_rm = TRUE)
  df_adj <- df
  df_adj$num[8] <- 0
  df_adj$ls[[8]][5] <- 0
  ans_expected <-
    tibble::tibble(unique(df_adj["time"]),
      num = c(
        df_adj$num[1] + df_adj$num[2] + df_adj$num[3] + df_adj$num[4],
        df_adj$num[5] + df_adj$num[6] + df_adj$num[7] + df_adj$num[8]
      ),
      ls = list(
        df_adj$ls[[1]] + df_adj$ls[[2]] + df_adj$ls[[3]] + df_adj$ls[[4]],
        df_adj$ls[[5]] + df_adj$ls[[6]] + df_adj$ls[[7]] + df_adj$ls[[8]]
      )
    )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'aggregate_count' works with valid inputs - two variables, na_rm is FALSE", {
  set.seed(0)
  df <- expand.grid(
    age = 0:1,
    sex = c("Female", "Male"),
    time = 2000:2001,
    KEEP.OUT.ATTRS = FALSE
  )
  df$num <- rnorm(8)
  df$ls <- replicate(n = 8, rnorm(5), simplify = FALSE)
  ans_obtained <- aggregate_count(df, collapse = c("age", "sex"), na_rm = FALSE)
  ans_expected <- tibble::tibble(unique(df["time"]),
    num = c(
      df$num[1] + df$num[2] + df$num[3] + df$num[4],
      df$num[5] + df$num[6] + df$num[7] + df$num[8]
    ),
    ls = list(
      df$ls[[1]] + df$ls[[2]] + df$ls[[3]] + df$ls[[4]],
      df$ls[[5]] + df$ls[[6]] + df$ls[[7]] + df$ls[[8]]
    )
  )
  expect_equal(ans_obtained, ans_expected)
})



## 'aggregate_rate' ----------------------------------------------------------

test_that("'aggregate_rate' works with valid inputs - na_rm is FALSE", {
  set.seed(0)
  df <- expand.grid(
    age = 0:1,
    sex = c("Female", "Male"),
    time = 2000:2001,
    KEEP.OUT.ATTRS = FALSE
  )
  df$wt <- replicate(n = 8, rnorm(5), simplify = FALSE)
  df$wt[[8]][5] <- NA
  df$unwt <- replicate(n = 8, rnorm(5), simplify = FALSE)
  ex <- replicate(n = 8, runif(5), simplify = FALSE)
  ans_obtained <- aggregate_rate(df,
    collapse = "age",
    unweighted = "unwt",
    exposure = ex,
    na_rm = FALSE
  )
  ans_expected <- tibble::tibble(unique(df[c("sex", "time")]),
    wt = list(
      (df$wt[[1]] * ex[[1]] + df$wt[[2]] * ex[[2]]) /
        (ex[[1]] + ex[[2]]),
      (df$wt[[3]] * ex[[3]] + df$wt[[4]] * ex[[4]]) /
        (ex[[3]] + ex[[4]]),
      (df$wt[[5]] * ex[[5]] + df$wt[[6]] * ex[[6]]) /
        (ex[[5]] + ex[[6]]),
      (df$wt[[7]] * ex[[7]] + df$wt[[8]] * ex[[8]]) /
        (ex[[7]] + ex[[8]])
    ),
    unwt = list(
      (df$unwt[[1]] + df$unwt[[2]]),
      (df$unwt[[3]] + df$unwt[[4]]),
      (df$unwt[[5]] + df$unwt[[6]]),
      (df$unwt[[7]] + df$unwt[[8]])
    )
  )
  expect_equal(ans_obtained, ans_expected)
})

test_that("'aggregate_rate' works with valid inputs - na_rm is TRUE", {
  set.seed(0)
  df <- expand.grid(
    age = 0:1,
    sex = c("Female", "Male"),
    time = 2000:2001,
    KEEP.OUT.ATTRS = FALSE
  )
  df$wt <- replicate(n = 8, rnorm(5), simplify = FALSE)
  df$wt[[8]][5] <- NA
  df$unwt <- replicate(n = 8, rnorm(5), simplify = FALSE)
  ex <- replicate(n = 8, runif(5), simplify = FALSE)
  ans_obtained <- aggregate_rate(df,
    collapse = "age",
    unweighted = "unwt",
    exposure = ex,
    na_rm = TRUE
  )
  df$wt[[8]][5] <- 0
  ans_expected <- tibble::tibble(unique(df[c("sex", "time")]),
    wt = list(
      (df$wt[[1]] * ex[[1]] + df$wt[[2]] * ex[[2]]) /
        (ex[[1]] + ex[[2]]),
      (df$wt[[3]] * ex[[3]] + df$wt[[4]] * ex[[4]]) /
        (ex[[3]] + ex[[4]]),
      (df$wt[[5]] * ex[[5]] + df$wt[[6]] * ex[[6]]) /
        (ex[[5]] + ex[[6]]),
      (df$wt[[7]] * ex[[7]] + df$wt[[8]] * ex[[8]]) /
        (ex[[7]] + ex[[8]])
    ),
    unwt = list(
      (df$unwt[[1]] + df$unwt[[2]]),
      (df$unwt[[3]] + df$unwt[[4]]),
      (df$unwt[[5]] + df$unwt[[6]]),
      (df$unwt[[7]] + df$unwt[[8]])
    )
  )
  expect_equal(ans_obtained, ans_expected)
})


## 'draw_counts' --------------------------------------------------------------

test_that("'draw_counts works with 'stk_init' uncertain - no adj needed", {
  set.seed(0)
  n_draw <- 5L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K + 1))
  sd <- diag(seq(from = 0.3, length.out = 2 * K + 1, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  val_dth <- rep(1, times = K)
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
  expect_identical(ans$n_adj, 0L)
})

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

test_that("'draw_counts_inner works with 'stk_init' known - no adj needed", {
  set.seed(0)
  n_draw <- 5L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K))
  sd <- diag(seq(from = 0.3, length.out = 2 * K, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  stk_init <- 7
  val_dth <- rep(1, times = K)
  ans <- draw_counts(
    n_draw = n_draw,
    mean = mean,
    var = var,
    stk_init = stk_init,
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
  expect_identical(ans$n_adj, 0L)
})

test_that("'draw_counts_inner works with 'stk_init' known - adj needed", {
  set.seed(0)
  n_draw <- 5L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K, by = 10))
  sd <- diag(seq(from = 0.3, length.out = 2 * K, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  stk_init <- 7
  val_dth <- rep(1, times = K)
  ans <- draw_counts(
    n_draw = n_draw,
    mean = mean,
    var = var,
    stk_init = stk_init,
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



## 'draw_counts_inner' --------------------------------------------------------

test_that("'draw_counts_inner works with 'stk_init' uncertain - no adjustment", {
  set.seed(0)
  n_draw <- 5L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K + 1, by = -0.1))
  sd <- diag(seq(from = 0.3, length.out = 2 * K + 1, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  val_dth <- rep(1, times = K)
  ans <- draw_counts_inner(
    n_draw = n_draw,
    mean = mean,
    var = var,
    val_dth = val_dth,
    use_adj = FALSE
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
  expect_true(ncol(ans$val_stk) <= n_draw)
})

test_that("'draw_counts_inner works with 'stk_init' uncertain - with adjustment", {
  set.seed(0)
  n_draw <- 5L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K + 1, by = -0.1))
  sd <- diag(seq(from = 0.3, length.out = 2 * K + 1, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  val_dth <- rep(1, times = K)
  ans <- draw_counts_inner(
    n_draw = n_draw,
    mean = mean,
    var = var,
    val_dth = val_dth,
    use_adj = TRUE
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
})

test_that("'draw_counts_inner works with 'stk_init' known - no adjustment", {
  set.seed(0)
  n_draw <- 5L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K, by = -0.3))
  sd <- diag(seq(from = 0.3, length.out = 2 * K, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  stk_init <- 7
  val_dth <- rep(1, times = K)
  ans <- draw_counts_inner(
    n_draw = n_draw,
    mean = mean,
    var = var,
    stk_init = stk_init,
    val_dth = val_dth,
    use_adj = FALSE
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
  expect_true(ncol(ans$val_stk) <= n_draw)
})

test_that("'draw_counts_inner works with 'stk_init' known - with adjustment", {
  set.seed(0)
  n_draw <- 5L
  K <- 6
  mean <- log(seq(from = 11, length.out = 2 * K, by = -0.3))
  sd <- diag(seq(from = 0.3, length.out = 2 * K, by = 0.1))
  sd[sd == 0] <- 0.05
  var <- crossprod(sd)
  stk_init <- 7
  val_dth <- rep(1, times = K)
  ans <- draw_counts_inner(
    n_draw = n_draw,
    mean = mean,
    var = var,
    stk_init = stk_init,
    val_dth = val_dth,
    use_adj = TRUE
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
})


## 'draw_par_datamods' --------------------------------------------------------

test_that("'draw_par_datamods' works", {
  set.seed(0)
  x <- sim_comod(scale_sd = 0.1)
  x <- fit(x)
  comp_acc <- components_cohort_account(x, n_draw = 10000)
  set.seed(1)
  ans <- draw_par_datamods(
    object = x,
    population = comp_acc$population,
    events = comp_acc$events
  )
  expect_equal(rowMeans(ans),
    x$mean[42:44],
    tolerance = 0.01
  )
})


## 'draw_rates' ---------------------------------------------------------------

test_that("'draw_rates' works - births", {
  set.seed(0)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_bth,
    events = sapply(comp$events$births, sum),
    exposure = comp$exposure$exposure,
    n_draw = n_draw
  )
  set.seed(1)
  ans_expected <- rgamma(
    n = length(x$sysmod_bth$mean) * n_draw,
    shape = rep((1 / x$sysmod_bth$disp), each = n_draw) +
      rep(sapply(comp$events$births, sum), each = n_draw),
    rate = rep((1 / x$sysmod_bth$disp), each = n_draw) *
      rep((1 / x$sysmod_bth$mean), each = n_draw) +
      unlist(comp$exposure$exposure)
  )
  expect_equal(unlist(ans_obtained), ans_expected)
})


test_that("'draw_rates' works - events certain", {
  set.seed(0)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_dth,
    events = comp$events$deaths,
    exposure = comp$exposure$exposure,
    n_draw = n_draw
  )
  set.seed(1)
  ans_expected <- rgamma(
    n = length(x$sysmod_dth$mean) * n_draw,
    shape = rep((1 / x$sysmod_dth$disp), each = n_draw) +
      rep(comp$events$deaths, each = n_draw),
    rate = rep((1 / x$sysmod_dth$disp), each = n_draw) *
      rep((1 / x$sysmod_dth$mean), each = n_draw) +
      unlist(comp$exposure$exposure)
  )
  expect_equal(unlist(ans_obtained), ans_expected)
})

test_that("'draw_rates' works - events uncertain", {
  set.seed(0)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_dth,
    events = comp$events$outs,
    exposure = comp$exposure$exposure,
    n_draw = n_draw
  )
  set.seed(1)
  ans_expected <- rgamma(
    n = length(x$sysmod_outs$mean) * n_draw,
    shape = rep((1 / x$sysmod_outs$disp), each = n_draw) +
      unlist(comp$events$outs),
    rate = rep((1 / x$sysmod_outs$disp), each = n_draw) *
      rep((1 / x$sysmod_outs$mean), each = n_draw) +
      unlist(comp$exposure$exposure)
  )
  expect_equal(unlist(ans_obtained), ans_expected)
})

test_that("'draw_rates' works - no exposure", {
  set.seed(0)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_ins,
    events = comp$events$ins,
    exposure = NULL,
    n_draw = n_draw
  )
  set.seed(1)
  ans_expected <- rgamma(
    n = length(x$sysmod_ins$mean) * n_draw,
    shape = rep((1 / x$sysmod_ins$disp), each = n_draw) +
      unlist(comp$events$ins),
    rate = rep((1 / x$sysmod_ins$disp), each = n_draw) *
      rep((1 / x$sysmod_ins$mean), each = n_draw) + 0.5
  )
  expect_equal(unlist(ans_obtained), ans_expected)
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


## 'get_best_value_multi' -----------------------------------------------------

test_that("'get_best_value_multi' works with valid inputs", {
  df1 <- expand.grid(age = 0:1, sex = c("Female", "Male"))
  df2 <- expand.grid(age = 0:2, time = 2000:2001)
  df3 <- expand.grid(age = 0:1, sex = c("Female", "Male"))
  df4 <- expand.grid(age = 0:2, time = 2000:2001)
  df1$val <- 1:4
  df2$val <- 11:16
  df3$val <- 21:24
  df4$val <- 31:36
  dfs <- list(df1, df2, df3, df4)
  ans_obtained <- get_best_value_multi(dfs)
  ans_expected <- suppressWarnings(
    merge(
      merge(merge(df1, df2, by = "age", all = TRUE),
        df3,
        by = c("age", "sex"), all = TRUE
      ),
      df4,
      by = c("age", "time"), all = TRUE
    )
  )
  ans_expected <- ans_expected[c("age", "time", "sex")]
  ans_expected <- ans_expected[with(ans_expected, order(age, time, sex)), ]
  ans_expected$value <- c(1L, 3L, 1L, 3L, 2L, 4L, 2L, 4L, 13L, 16L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_best_value_multi' works with single data frame", {
  df1 <- expand.grid(
    age = 0:1, sex = c("Female", "Male"),
    KEEP.OUT.ATTRS = FALSE
  )
  df1$val <- 1:4
  dfs <- list(df1)
  ans_obtained <- get_best_value_multi(dfs)
  ans_expected <- df1
  names(ans_expected)[[3]] <- "value"
  expect_identical(ans_obtained, ans_expected)
})


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


## 'is_positive_definite' -----------------------------------------------------

test_that("'is_positive_definite' works with valid inputs", {
  set.seed(0)
  n <- 10
  ans <- logical(n)
  for (i in 1:n) {
    a <- matrix(rnorm(20), nr = 4)
    m <- crossprod(a)
    ans[i] <- is_positive_definite(m)
  }
  expect_true(all(ans))
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


## 'make_fail_mean' -----------------------------------------------------------

test_that("'make_fail_mean' works with valid inputs", {
  parameters <- list(
    log_stk_init = -1,
    log_val_ins = c(0.1, 0.2),
    log_val_outs = c(0.2, 0.3)
  )
  ans_obtained <- make_fail_mean(parameters)
  ans_expected <- c(
    log_stk_init = NA_real_,
    log_val_ins = NA_real_,
    log_val_ins = NA_real_,
    log_val_outs = NA_real_,
    log_val_outs = NA_real_
  )
  expect_identical(ans_obtained, ans_expected)
})


## 'make_fail_var' ------------------------------------------------------------

test_that("'make_fail_var' works with valid inputs", {
  parameters <- list(
    log_stk_init = -1,
    log_val_ins = c(0.1, 0.2),
    log_val_outs = c(0.2, 0.3)
  )
  ans_obtained <- make_fail_var(parameters)
  nms <- c(
    "log_stk_init",
    "log_val_ins",
    "log_val_ins",
    "log_val_outs",
    "log_val_outs"
  )
  ans_expected <- matrix(0, nr = 5, nc = 5, dimnames = list(nms, nms))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_labels_codatamods' ---------------------------------------------------

test_that("'make_labels_codatamods' works with valid inputs", {
  args <- data.frame(
    data = as.double(1:10),
    age = rep(0:4, each = 2),
    time = c(2000L, rep(2001:2004, each = 2), 2005L),
    is_obs = c(0L, rep(1L, 9)),
    ratio = rep(1, 10),
    sd = rep(0.25, 10),
    scale_ratio = rep(0, 10),
    scale_sd = rep(0.1, 10)
  )
  x1 <- new_codatamod_norm(args)
  x2 <- new_codatamod_norm(args)
  x2$data <- as.double(101:110)
  mods <- list(df1 = x1, df2 = x2)
  ans_obtained <- make_labels_codatamods(mods, nm_series = "ins")
  ans_expected <- tibble::tibble(
    series = "ins",
    data = c("df1", "df2"),
    par = c("mult_sd", "mult_sd")
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


## 'matrix_to_list_rows' ------------------------------------------------------

test_that("'matrix_to_list_rows' works with valid inputs", {
  m <- matrix(1:6, nr = 2, dimnames = list(x = 1:2, b = 1:3))
  ans_obtained <- matrix_to_list_rows(m)
  ans_expected <- list(c(1L, 3L, 5L), c(2L, 4L, 6L))
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


## 'nest_to_list' -------------------------------------------------------------

test_that("'nest_to_list' works with valid inputs - data is atomic", {
  df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2001:2005)
  df$count <- seq_len(nrow(df))
  ans <- nest_to_list(df,
    nm_data = "count",
    nm_id = "time",
    nms_group = c("sex", "age"),
    nm_value = "val"
  )
  expect_identical(names(ans), c("sex", "age", "val"))
  expect_identical(
    unname(sapply(ans, class)),
    c("character", "integer", "list")
  )
  ans_obtained <- ans$val[[1L]]
  ans_expected <- df[df$age == 0 & df$sex == "F", "count"]
  names(ans_expected) <- 2001:2005
  expect_identical(ans_obtained, ans_expected)
})

test_that("'nest_to_list' works with valid inputs - data is list", {
  df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2001:2005)
  df$count <- as.list(seq_len(nrow(df)))
  ans <- nest_to_list(df,
    nm_data = "count",
    nm_id = "time",
    nms_group = c("sex", "age"),
    nm_value = "val"
  )
  expect_identical(names(ans), c("sex", "age", "val"))
  expect_identical(
    unname(sapply(ans, class)),
    c("character", "integer", "list")
  )
  ans_obtained <- ans$val[[1L]]
  ans_expected <- df[df$age == 0 & df$sex == "F", "count"]
  names(ans_expected) <- 2001:2005
  expect_identical(ans_obtained, ans_expected)
  expect_true(is.list(ans_obtained))
})


## 'pivot_sex_index_births' ---------------------------------------------------

test_that("'pivot_sex_index_births' works with single classif variable", {
  df <- data.frame(sex = c("F", "M", "O"))
  df$births <- list(
    c(M = 1, O = 2, F = 3),
    c(M = NA_real_, O = NA_real_, F = NA_real_),
    c(M = NA_real_, O = NA_real_, F = NA_real_)
  )
  ans_obtained <- pivot_sex_index_births(df = df, nm_bth = "births")
  ans_expected <- data.frame(
    sex = c("F", "M", "O"),
    births = c(3, 1, 2)
  )
  expect_identical(ans_obtained, ans_expected)
})


test_that("'pivot_sex_index_births' works with multiple classif variable", {
  df <- data.frame(
    sex = c("F", "M", "O", "F", "M", "O"),
    age = rep(1:2, each = 3)
  )
  df$births <- list(
    c(M = NA_real_, O = NA_real_, F = NA_real_),
    c(M = NA_real_, O = NA_real_, F = NA_real_),
    c(M = NA_real_, O = NA_real_, F = NA_real_),
    c(M = 1, O = 2, F = 3),
    c(M = NA_real_, O = NA_real_, F = NA_real_),
    c(M = NA_real_, O = NA_real_, F = NA_real_)
  )
  ans_obtained <- pivot_sex_index_births(df = df, nm_bth = "births")
  ans_expected <- data.frame(
    sex = c("F", "M", "O"),
    age = rep(1:2, each = 3),
    births = c(NA, NA, NA, 3, 1, 2)
  )
  expect_identical(ans_obtained, ans_expected)
})



## 'project_cohort' -----------------------------------------------------------

test_that("'project_cohort' works with new cohorts", {
  set.seed(0)
  prior_stk_init <- list(
    mean = 100,
    sd = 0
  )
  sysmod_bth <- list(
    mean = rep(0, 20),
    disp = rep(0, 20)
  )
  sysmod_dth <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  sysmod_ins <- list(
    mean = rep(4, 20),
    disp = rep(0.1, 20)
  )
  sysmod_outs <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  ans <- project_cohort(
    is_new_cohort = TRUE,
    prior_stk_init = prior_stk_init,
    sysmod_bth = sysmod_bth,
    sysmod_dth = sysmod_dth,
    sysmod_ins = sysmod_ins,
    sysmod_outs = sysmod_outs
  )
  expect_true(identical(length(ans$val_stk), 21L))
  expect_true(identical(length(ans$val_bth), 20L))
  expect_true(all(unlist(ans[c("val_dth", "val_ins", "val_outs")]) >= 0))
  expect_true(any(unlist(ans[c("val_dth", "val_ins", "val_outs")]) > 0))
  expect_identical(
    diff(ans$val_stk),
    ans$val_ins - ans$val_dth - ans$val_outs
  )
})

test_that("'project_cohort' works with existing cohorts", {
  set.seed(0)
  prior_stk_init <- list(
    mean = 100,
    sd = Inf
  )
  sysmod_bth <- list(
    mean = rep(0, 20),
    disp = rep(0, 20)
  )
  sysmod_dth <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  sysmod_ins <- list(
    mean = rep(4, 20),
    disp = rep(0.1, 20)
  )
  sysmod_outs <- list(
    mean = rep(0.02, 20),
    disp = rep(0.1, 20)
  )
  ans <- project_cohort(
    is_new_cohort = FALSE,
    prior_stk_init = prior_stk_init,
    sysmod_bth = sysmod_bth,
    sysmod_dth = sysmod_dth,
    sysmod_ins = sysmod_ins,
    sysmod_outs = sysmod_outs
  )
  expect_true(identical(length(ans$val_stk), 21L))
  expect_true(identical(length(ans$val_bth), 20L))
  expect_true(all(unlist(ans) >= 0))
  expect_true(any(unlist(ans) > 0))
  expect_identical(
    diff(ans$val_stk),
    ans$val_ins - ans$val_dth - ans$val_outs
  )
})


## 'sim_time_age' -------------------------------------------------------------

test_that("'sim_time_age' works with new cohorts", {
  expect_identical(
    sim_time_age(K = 1L, cohort = 2015L, is_new_cohort = TRUE),
    data.frame(time = 2015L, age = 0L)
  )
  expect_identical(
    sim_time_age(K = 2L, cohort = 2015L, is_new_cohort = TRUE),
    data.frame(time = 2015:2016, age = c(0L, 0L))
  )
  expect_identical(
    sim_time_age(K = 4L, cohort = 2015L, is_new_cohort = TRUE),
    data.frame(
      time = c(2015:2016, 2016:2017),
      age = c(0L, 0L, 1L, 1L)
    )
  )
})

test_that("'sim_time_age' works with existing cohorts", {
  expect_identical(
    sim_time_age(K = 1L, cohort = 1990L, is_new_cohort = FALSE),
    data.frame(time = 2021L, age = 30L)
  )
  expect_identical(
    sim_time_age(K = 2L, cohort = 1990L, is_new_cohort = FALSE),
    data.frame(time = c(2021L, 2021L), age = 30:31)
  )
  expect_identical(
    sim_time_age(K = 4L, cohort = 1990L, is_new_cohort = FALSE),
    data.frame(
      time = c(2021L, 2021L, 2022L, 2022L),
      age = c(30:31, 31:32)
    )
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
  ans_obtained <- summarise_estimated_means(mean = mean, prior_stk_init = prior_stk_init)
  ans_expected <- data.frame(
    mean_stk_end = c(
      exp(4) + exp(-1) - exp(1),
      exp(4) + exp(-1) + exp(-2) -
        exp(1) - exp(2)
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
  ans_obtained <- summarise_estimated_means(mean = mean, prior_stk_init = prior_stk_init)
  ans_expected <- data.frame(
    mean_stk_end = c(
      10 + exp(-1) - exp(1),
      10 + exp(-1) + exp(-2) -
        exp(1) - exp(2)
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


## 'unpivot_sex_index_births' -------------------------------------------------

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


## 'whole_num_halve' ----------------------------------------------------------

test_that("'whole_num_halve' works with even numbers", {
  expect_equal(whole_num_halve(4), 2)
  expect_equal(whole_num_halve(0), 0)
  expect_equal(whole_num_halve(10000), 5000)
})

test_that("'whole_num_halve' works with odd numbers", {
  set.seed(0)
  ans <- replicate(n = 1000, whole_num_halve(5))
  expect_equal(mean(ans), 2.5, tolerance = 0.01)
})
