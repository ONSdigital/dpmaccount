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

test_that("'add_summaries' throws correct error with wrong column name", {
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
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod(scale_sd = 0.1)
  x <- fit(x)
  comp_acc <- components_cohort_account(x, n_draw = 10000, seed_list = seed_list)
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

test_that("'draw_rates' throws error if events not numeric/list/vector", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  expect_error(draw_rates(
    sysmod = x$sysmod_bth,
    events = as.character(sapply(comp$events$births, sum)),
    exposure = comp$exposure$exposure,
    n_draw = n_draw
  ), "'events' has class")
})


test_that("'draw_rates' throws error if exposure not numeric/list/vector", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  expect_error(draw_rates(
    sysmod = x$sysmod_bth,
    events = sapply(comp$events$births, sum),
    exposure = as.character(comp$exposure$exposure),
    n_draw = n_draw
  ), "'exposure' has class")
})

test_that("'draw_rates' works - births", {
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_bth,
    events = sapply(comp$events$births, sum),
    exposure = comp$exposure$exposure,
    n_draw = n_draw,
    seed_in = 1
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
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_dth,
    events = comp$events$deaths,
    exposure = comp$exposure$exposure,
    n_draw = n_draw,
    seed_in = 1
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
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_dth,
    events = comp$events$outs,
    exposure = comp$exposure$exposure,
    n_draw = n_draw,
    seed_in = 1
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
  seed_list <- make_seed_account(seed_in = 1)
  x <- sim_comod()
  x <- fit(x)
  n_draw <- 10
  comp <- components_cohort_account(x, n_draw = n_draw, seed_list = seed_list)
  set.seed(1)
  ans_obtained <- draw_rates(
    sysmod = x$sysmod_ins,
    events = comp$events$ins,
    exposure = NULL,
    n_draw = n_draw,
    seed_in = 1
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


## 'matrix_to_list_rows' ------------------------------------------------------

test_that("'matrix_to_list_rows' works with valid inputs", {
  m <- matrix(1:6, nr = 2, dimnames = list(x = 1:2, b = 1:3))
  ans_obtained <- matrix_to_list_rows(m)
  ans_expected <- list(c(1L, 3L, 5L), c(2L, 4L, 6L))
  expect_identical(ans_obtained, ans_expected)
})


## 'pivot_sex_index_births' ---------------------------------------------------

test_that("'pivot_sex_index_births' throws error if passed data frame without sex variable", {
  df <- data.frame(gender = c("F", "M", "O"))
  df$births <- list(
    c(M = 1, O = 2, F = 3),
    c(M = NA_real_, O = NA_real_, F = NA_real_),
    c(M = NA_real_, O = NA_real_, F = NA_real_)
  )

  expect_error(
    pivot_sex_index_births(df = df, nm_bth = "births"),
    "data frame does not contain 'sex' variable"
  )
})

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
