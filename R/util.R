#' Add posterior mean and 95 percent credible interval
#' for variable
#'
#' Given a list column 'vname', add columns
#' 'vname.fitted', 'vname.lower', and 'vname.upper'
#' to a data frame, giving the posterior mean and
#' lower and upper limits of credible intervals
#'
#' @param df A data frame
#' @param vname The name of the list column
#' @param width Width of credible intervals
#'
#' @returns A modified version of 'df',
#' with three extra columns.
#'
#' @noRd
add_summaries <- function(df, vname, width) {
  probs <- c(
    (1 - width) / 2,
    (1 + width) / 2
  )
  nms_df <- names(df)
  if (!(vname %in% nms_df)) {
    stop(
      gettextf(
        "data frame does not contain a variable called '%s'",
        vname
      ),
      call. = FALSE
    )
  }
  var <- df[[vname]]
  if (!is.list(var)) {
    stop(
      gettextf(
        "'%s' is not a list column",
        vname
      ),
      call. = FALSE
    )
  }
  var <- do.call(rbind, var)
  means <- matrixStats::rowMeans2(var)
  quantiles <- matrixStats::rowQuantiles(var, probs = probs)
  df[[paste0(vname, ".fitted")]] <- means
  df[[paste0(vname, ".lower")]] <- quantiles[, 1L]
  df[[paste0(vname, ".upper")]] <- quantiles[, 2L]
  df
}


## HAS_TESTS
#' Adjust a single draw of ins, outs, and possibly initial population,
#' so that subsequent stock values all non-negative
#'
#' Find values as close as possible to initial estimate that
#' satisfy the non-negativity constraint on stock at the
#' end of each triangle.
#'
#' @param val_stk_init. Initial value for stock.
#' Non-negative scalar double.
#' @param val_ins Inward moves. Vector of positive doubles.
#' @param val_outs Outward moves. Vector of positive doubles.
#' @param is_known_stk_init Whether 'val_stk_init' should be
#' treated as known, and hence fixed.
#'
#' @returns List with components 'val_stk_init', 'val_ins'
#' and 'val_outs'. In general, 'val_ins' and 'val_outs' will
#' differ from their original values. 'val_stk_init' will
#' may differ from its original value if 'is_known_stk_init'
#' is FALSE.
#'
#' @noRd
adjust_counts <- function(val_stk_init,
                          val_dth,
                          val_ins,
                          val_outs,
                          is_known_stk_init) {
  K <- length(val_ins)
  val_stk_end <- val_stk_init - cumsum(val_dth) + cumsum(val_ins) - cumsum(val_outs)
  if (all(val_stk_end >= 0)) {
    ans <- list(
      val_stk_init = val_stk_init,
      val_stk = val_stk_end,
      val_dth = val_dth,
      val_ins = val_ins,
      val_outs = val_outs
    )
    return(ans)
  }
  f <- function(x) sum((x - val_old)^2 / (val_old + 1)^2)
  grad <- function(x) 2 * (x - val_old) / (val_old + 1)^2
  if (is_known_stk_init) {
    val_old <- c(val_ins, val_outs)
    theta <- c(val_dth + 1, rep(0.5, times = K))
    C <- matrix(0L, nrow = K, ncol = K)
    C[row(C) >= col(C)] <- 1L
    ui <- cbind(C, -C)
    ui <- rbind(ui, diag(2L * K))
    ci <- c(cumsum(val_dth) - val_stk_init, rep(0, times = 2L * K))
    out <- stats::constrOptim(
      theta = theta,
      f = f,
      grad = grad,
      ui = ui,
      ci = ci
    )
    par <- out$par
    val_ins <- par[1:K]
    val_outs <- par[(K + 1L):(2L * K)]
  } else {
    val_old <- c(val_stk_init, val_ins, val_outs)
    theta <- c(val_stk_init + 0.5, val_dth + 1, rep(0.5, times = K))
    C <- matrix(0L, nrow = K, ncol = K)
    C[row(C) >= col(C)] <- 1L
    ui <- cbind(C, -C)
    ui <- rbind(0, ui)
    ui <- cbind(1, ui)
    ui <- rbind(ui, diag(2L * K + 1L))
    ci <- c(0, cumsum(val_dth), rep(0, times = 2L * K + 1L))
    out <- stats::constrOptim(
      theta = theta,
      f = f,
      grad = grad,
      ui = ui,
      ci = ci
    )
    par <- out$par
    val_stk_init <- par[[1L]]
    val_ins <- par[2L:(K + 1L)]
    val_outs <- par[(K + 2L):(2L * K + 1L)]
  }
  val_stk <- val_stk_init - cumsum(val_dth) + cumsum(val_ins) - cumsum(val_outs)
  ans <- list(
    val_stk_init = val_stk_init,
    val_stk = val_stk,
    val_dth = val_dth,
    val_ins = val_ins,
    val_outs = val_outs
  )
  ans
}


## HAS_TESTS
#' Aggregate across classification variable(s) in a data frame of counts
#'
#' Aggregate count measure variables, while dropping one or more
#' classification variables. Works on numeric or list versions
#' of measure variables.
#'
#' @param df A data frame
#' @param collapse Name(s) of the classification variable(s)
#' to collapse.
#' One or more from `"age"`, `"sex"`, and `"cohort"`.
#' (Not `"time"`)
#' @param na_rm Whether to convert `NA`s to 0s before
#' summing.
#'
#' @returns A smaller data frame
#'
#' @noRd
aggregate_count <- function(df, collapse, na_rm) {
  nms_classif_var <- c("age", "sex", "cohort", "time")
  nms_df <- names(df)
  nms_measure_var <- setdiff(nms_df, nms_classif_var)
  nms_by <- intersect(setdiff(nms_classif_var, collapse), nms_df)
  sum_num_or_list <- function(x) {
    if (is.numeric(x)) {
      sum(x, na.rm = na_rm)
    } else {
      if (na_rm) {
        x <- lapply(
          x,
          function(v) {
            v[is.na(v)] <- 0
            v
          }
        )
      }
      list(Reduce("+", x))
    }
  }
  ans <- stats::aggregate(df[nms_measure_var],
    by = df[nms_by],
    FUN = sum_num_or_list
  )
  ans <- tibble::as_tibble(ans)
  ans
}


## HAS_TESTS
#' Aggregate across classification variable(s) in a data frame of rates
#'
#' Aggregate raate measure variables, while dropping one or more
#' classification variables. Requires a vector of exposures.
#' Rates and exposure assumed to be lists.
#'
#' @param df A data frame
#' @param collapse Name(s) of the classification variable(s)
#' to collapse. One or more from "age", "sex", "cohort", and "time".
#' @param unweighted Names of measure variables that should
#' be added without weights.
#' @param exposure A list of exposure vectors.
#' @param na_rm Whether to convert `NA`s to 0s before
#' summing.
#'
#' @returns A smaller data frame
#'
#' @noRd
aggregate_rate <- function(df, collapse, unweighted, exposure, na_rm) {
  nms_classif_var <- c("age", "sex", "cohort", "time")
  nms_df <- names(df)
  nms_measure_var <- setdiff(nms_df, nms_classif_var)
  nms_weight <- setdiff(nms_measure_var, unweighted)
  counts <- df
  for (nm in nms_weight) {
    counts[[nm]] <- .mapply("*",
      dots = list(df[[nm]], exposure),
      MoreArgs = list()
    )
  }
  ans <- aggregate_count(counts,
    collapse = collapse,
    na_rm = na_rm
  )
  exposure_df <- data.frame(df[intersect(nms_df, nms_classif_var)])
  exposure_df$exposure <- exposure
  exposure_ag <- aggregate_count(exposure_df,
    collapse = collapse,
    na_rm = na_rm
  )
  for (nm in nms_weight) {
    ans[[nm]] <- .mapply("/",
      dots = list(ans[[nm]], exposure_ag$exposure),
      MoreArgs = list()
    )
  }
  ans
}


## HAS_TESTS
#' Draw counts of population, deaths, migration
#'
#' Draw demographic counts, given a mean vector
#' and variance matrix for log migration and
#' possibly log initial population.
#' Keep drawing until all stock values are non-negative.
#'
#' @param n_draw Number of random draws
#' @param mean Mean for distribution of log counts
#' @param var Variance for distribution of log counts
#' @param stk_init Initial value for population, if
#' being treated as known (so not included in
#' 'mean' and 'var')
#' @param var_dth Values for deaths, treated as known.
#' @param cohort Year of birth of cohort (used in error messages)
#' @param sex Sex of cohort (used in error messages)
#'
#' @returns A named list
#'
#' @noRd
draw_counts <- function(n_draw, mean, var, stk_init = NULL, val_dth,
                        cohort, sex) {
  iter_max <- 100L
  K <- length(val_dth)
  ans <- list(
    val_stk_init = NULL,
    val_stk = NULL,
    val_dth = NULL,
    val_ins = NULL,
    val_outs = NULL
  )
  n_needed <- n_draw
  n_draw_iter <- n_draw
  for (iter in seq_len(iter_max)) {
    draws <- draw_counts_inner(
      n_draw = n_draw_iter,
      mean = mean,
      var = var,
      stk_init = stk_init,
      val_dth = val_dth,
      use_adj = FALSE
    )
    n_success <- length(draws$val_stk_init)
    n_keep <- min(n_success, n_needed)
    i_keep <- seq_len(n_keep)
    ans <- list(
      val_stk_init = c(ans$val_stk_init, draws$val_stk_init[i_keep]),
      val_stk = cbind(ans$val_stk, draws$val_stk[, i_keep, drop = FALSE]),
      val_dth = cbind(ans$val_dth, draws$val_dth[, i_keep, drop = FALSE]),
      val_ins = cbind(ans$val_ins, draws$val_ins[, i_keep, drop = FALSE]),
      val_outs = cbind(ans$val_outs, draws$val_outs[, i_keep, drop = FALSE]),
      n_adj = 0L
    )
    if (n_success >= n_needed) {
      return(ans)
    }
    if (iter == 1L) {
      if (n_success == 0L) {
        prob_success <- 0.05
      } else {
        prob_success <- n_success / n_draw
      }
    }
    n_needed <- n_needed - n_keep
    n_draw_iter <- ceiling(n_needed / prob_success)
  }
  draws <- draw_counts_inner(
    n_draw = n_needed,
    mean = mean,
    var = var,
    stk_init = stk_init,
    val_dth = val_dth,
    use_adj = TRUE
  )
  ans <- list(
    val_stk_init = c(ans$val_stk_init, draws$val_stk_init),
    val_stk = cbind(ans$val_stk, draws$val_stk),
    val_dth = cbind(ans$val_dth, draws$val_dth),
    val_ins = cbind(ans$val_ins, draws$val_ins),
    val_outs = cbind(ans$val_outs, draws$val_outs),
    n_adj = n_needed
  )
  ans
}


## HAS_TESTS
#' Draw counts of population, deaths, migration
#'
#' Draw demographic counts, given a mean vector
#' and variance matrix for log migration and
#' possibly log initial population.
#' Attempt to generate 'n_draw' draws, but only return
#' draw where all stock values are non-negative.
#'
#' @param n_draw Number of random draws
#' @param mean Mean for distribution of log counts
#' @param var Variance for distribution of log counts
#' @param stk_init Initial value for population, if
#' being treated as known (so not included in
#' 'mean' and 'var')
#' @param var_dth Values for deaths, treated as known.
#' @param use_adj Whether to adjust values if draw fails
#' to meet non-negativity constraint.
#'
#' @returns A named list
#'
#' @noRd
draw_counts_inner <- function(n_draw,
                              mean,
                              var,
                              stk_init = NULL,
                              val_dth,
                              use_adj) {
  draws_log_val <- MASS::mvrnorm(
    n = n_draw,
    mu = mean,
    Sigma = var
  )
  ## after transpose, each *column* is one draw from posterior
  draws_log_val <- t(draws_log_val)
  draws_val <- exp(draws_log_val)
  is_known_stk_init <- !is.null(stk_init)
  if (is_known_stk_init) {
    val_stk_init <- rep.int(stk_init, times = n_draw)
  } else {
    val_stk_init <- draws_val[1L, ]
  }
  K <- length(val_dth)
  val_stk_init_m <- matrix(val_stk_init,
    nrow = K,
    ncol = n_draw,
    byrow = TRUE
  )
  val_dth <- matrix(val_dth,
    nrow = K,
    ncol = n_draw
  )
  if (is_known_stk_init) {
    i_val_ins <- seq.int(from = 1L, length.out = K)
  } else {
    i_val_ins <- seq.int(from = 2L, length.out = K)
  }
  i_val_outs <- i_val_ins + K
  val_ins <- draws_val[i_val_ins, , drop = FALSE]
  val_outs <- draws_val[i_val_outs, , drop = FALSE]
  val_dth_cum <- matrixStats::colCumsums(val_dth)
  val_ins_cum <- matrixStats::colCumsums(val_ins)
  val_outs_cum <- matrixStats::colCumsums(val_outs)
  val_stk <- val_stk_init_m - val_dth_cum + val_ins_cum - val_outs_cum
  is_nonneg <- (val_stk_init >= 0) & matrixStats::colAlls(val_stk >= 0)
  if (use_adj) {
    for (i in seq_len(n_draw)) {
      if (!is_nonneg[[i]]) {
        val <- adjust_counts(
          val_stk_init = val_stk_init[[i]],
          val_dth = val_dth[, i],
          val_ins = val_ins[, i],
          val_outs = val_outs[, i],
          is_known_stk_init = is_known_stk_init
        )
        val_stk_init[i] <- val$val_stk_init
        val_stk[, i] <- val$val_stk
        val_dth[, i] <- val$val_dth
        val_ins[, i] <- val$val_ins
        val_outs[, i] <- val$val_outs
      }
    }
    ans <- list(
      val_stk_init = val_stk_init,
      val_stk = val_stk,
      val_dth = val_dth,
      val_ins = val_ins,
      val_outs = val_outs
    )
  } else {
    ans <- list(
      val_stk_init = val_stk_init[is_nonneg],
      val_stk = val_stk[, is_nonneg, drop = FALSE],
      val_dth = val_dth[, is_nonneg, drop = FALSE],
      val_ins = val_ins[, is_nonneg, drop = FALSE],
      val_outs = val_outs[, is_nonneg, drop = FALSE]
    )
  }
  ans
}


## HAS_TESTS
#' Draw values for parameters from data models
#'
#' Draws are conditional on draws for population
#' and events.
#'
#' Assume that at least one data model has parameters.
#'
#' @param object Fitted object of class 'dpmaccount_comod'
#' @param population Tibble with draws for population
#' @param events Tibble with draws for events
#'
#' @returns A matrix
#'
#' @noRd
draw_par_datamods <- function(object, population, events) {
  mean <- object$mean
  var <- object$var
  stk_init_sd <- object$prior_stk_init$sd
  is_known_stk_init <- isTRUE(all.equal(stk_init_sd, 0))
  ins <- events$ins
  outs <- events$outs
  mods <- c(
    object$datamods_stk,
    object$datamods_ins,
    object$datamods_outs
  )
  transform_datamod <- get_transform_datamod_all(mods)
  K <- length(ins)
  n_draw <- length(ins[[1L]])
  ins <- matrix(unlist(ins), nrow = K, ncol = n_draw)
  outs <- matrix(unlist(outs), nrow = K, ncol = n_draw)
  draws_account <- rbind(ins, outs)
  draws_account[draws_account < 1e-6] <- 1e-6
  draws_account <- log(draws_account)
  if (!is_known_stk_init) {
    stk_init <- population$population[[1L]]
    stk_init[stk_init < 1e-6] <- 1e-6
    stk_init <- log(stk_init)
    draws_account <- rbind(stk_init, draws_account)
  }
  is_par_datamod <- seq_along(mean) > (2L * K + !is_known_stk_init)
  n_par_datamod <- sum(is_par_datamod)
  mean_par_datamod <- (mean[is_par_datamod]
  + var[is_par_datamod, !is_par_datamod]
    %*% var[!is_par_datamod, !is_par_datamod]
    %*% (draws_account - mean[!is_par_datamod]))
  var_par_datamod <- (var[is_par_datamod, is_par_datamod]
  - var[is_par_datamod, !is_par_datamod]
    %*% solve(
      var[!is_par_datamod, !is_par_datamod],
      var[!is_par_datamod, is_par_datamod]
    ))
  R <- chol(var_par_datamod)
  Z <- matrix(stats::rnorm(n = n_par_datamod * n_draw),
    nrow = n_par_datamod,
    ncol = n_draw
  )
  ans <- mean_par_datamod + R %*% Z
  for (i_par_datamod in seq_len(n_par_datamod)) {
    transform <- transform_datamod[[i_par_datamod]]
    ans[i_par_datamod, ] <- transform(ans[i_par_datamod, ])
  }
  ans
}


## HAS_TESTS
#' Draw values for rates
#'
#' Draw values for rates, using gamma distribution.
#'
#' When exposure is NULL, implying that the
#' series is "ins", use an exposure of 1,
#' to represent one Lexis triangle,
#' implying that rates refer to a single Lexis triangle.
#'
#' @param sysmod A list with elements 'mean' and 'disp'
#' @param events A vector (if the events are treated as known)
#' or a list of vectors (if the events are uncertain)
#' @param exposure A list of vectors, or NULL.
#' @param n_draw Number of draws
#'
#' @returns A list of vectors.
#'
#' @noRd
draw_rates <- function(sysmod, events, exposure, n_draw) {
  mean <- sysmod$mean
  disp <- sysmod$disp
  if (is.numeric(events)) { ## events certain
    events <- rep(events, each = n_draw)
  } else if (is.list(events)) { ## events uncertain
    events <- unlist(events)
  } else {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        "events",
        class(events)
      ),
      call. = FALSE
    )
  }
  if (is.list(exposure)) { ## is "deaths", "births", "outs"
    exposure <- unlist(exposure)
  } else if (is.null(exposure)) { ## is "ins"
    exposure <- 0.5 ## one Lexis triangle
  } else {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        "exposure",
        class(exposure)
      ),
      call. = FALSE
    )
  }
  K <- length(mean)
  mean <- rep(mean, each = n_draw)
  disp <- rep(disp, each = n_draw)
  gamma_shape_prior <- 1 / disp
  gamma_rate_prior <- 1 / (mean * disp)
  gamma_shape_post <- gamma_shape_prior + events
  gamma_rate_post <- gamma_rate_prior + exposure
  ans <- matrix(NA_real_, nrow = n_draw, ncol = K)
  has_data <- !is.na(events) & !is.na(exposure)
  ans[has_data] <- stats::rgamma(
    n = sum(has_data),
    shape = gamma_shape_post[has_data],
    rate = gamma_rate_post[has_data]
  )
  ans <- matrix_to_list_cols(ans)
  ans
}



## HAS_TESTS
#' Check that all elements of a list are set-equal
#'
#' @param l A list
#'
#' @returns TRUE or FALSE
#'
#' @noRd
elements_setequal <- function(l) {
  n <- length(l)
  if (n < 2L) {
    return(TRUE)
  }
  el_1 <- l[[1L]]
  for (i in seq.int(from = 2L, to = n)) {
    el_i <- l[[i]]
    if (!setequal(el_1, el_i)) {
      return(FALSE)
    }
  }
  TRUE
}


## HAS_TESTS
#' Do a full join on two datasets, based on age and time
#'
#' Do a full join (ie a join keeping rows from x and y)
#' using "time" as 'by' column.
#'
#' @param x,y Data frames
#'
#' @returns A data frame.
#'
#' @noRd
full_join_time <- function(x, y) {
  merge(x, y, all.x = TRUE, all.y = TRUE, by = "time")
}


## HAS_TESTS
#' Do a full join on two datasets, based on age and time
#'
#' Do a full join (ie a join keeping rows from x and y)
#' using "time", and "age" as 'by' columns.
#'
#' @param x,y Data frames
#'
#' @returns A data frame.
#'
#' @noRd
full_join_timeage <- function(x, y) {
  merge(x, y, all.x = TRUE, all.y = TRUE, by = c("time", "age"))
}

## HAS_TESTS
#' Search across multiple datasets for measurements
#'
#' Search across multiple datasets for measures for each
#' combination of the classification variables.
#' Assume datasets ordered by reliability.
#'
#' @param dfs A list of data frames
#'
#' @return A data frame with classification variables
#' and the measurement variable.
#'
#' @noRd
get_best_value_multi <- function(dfs) {
  nms_classif <- c("cohort", "sex", "time", "age")
  full_join <- function(x, y) {
    by <- Reduce(intersect, list(names(x), names(y), nms_classif))
    merge(x, y, all = TRUE, by = by)
  }
  for (i in seq_along(dfs)) {
    nms_i <- names(dfs[[i]])
    is_measure_var <- !(nms_i %in% nms_classif)
    nms_i[is_measure_var] <- sprintf(".%s.%d", nms_i[is_measure_var], i)
    names(dfs[[i]]) <- nms_i
  }
  df <- Reduce(full_join, dfs)
  nms <- names(df)
  is_classif_var <- nms %in% nms_classif
  classif_vars <- df[is_classif_var]
  measure_vars <- df[!is_classif_var]
  value <- measure_vars[[1L]]
  n <- ncol(measure_vars)
  if (n > 1L) {
    for (i in seq.int(from = 2L, to = n)) {
      is_na <- is.na(value)
      if (sum(is_na) == 0L) {
        break
      }
      vals <- measure_vars[[i]]
      value[is_na] <- vals[is_na]
    }
  }
  ans <- classif_vars
  ans$value <- value
  ans
}


## HAS_TESTS
#' Search across signle dataset for measurements
#'
#' Search across multiple columns in a single dataset
#' for measures for each
#' combination of the classification variables.
#' Assume columns ordered from left to right
#' by reliability.
#'
#' @param df A data frame
#' @param nm Name of the measurement variable
#' in the result.
#'
#' @return A data frame with classification variables
#' and best estimate of measurement variable.
#'
#' @noRd
get_best_value_one <- function(df, nm) {
  nms_df <- names(df)
  nms_classif_vars <- c("age", "sex", "cohort", "time")
  nms_classif_obs <- intersect(nms_df, nms_classif_vars)
  nms_measure_vars <- setdiff(nms_df, nms_classif_vars)
  measure_vars <- df[nms_measure_vars]
  ans <- measure_vars[[1L]]
  n <- ncol(measure_vars)
  if (n > 1L) {
    for (i in seq.int(from = 2L, to = n)) {
      is_na <- is.na(ans)
      if (sum(is_na) == 0L) {
        break
      }
      val <- measure_vars[[i]]
      ans[is_na] <- val[is_na]
    }
  }
  ans <- cbind(df[nms_classif_obs], ans)
  names(ans)[length(ans)] <- nm
  ans
}


## HAS_TESTS
#' Test whether a matrix is positive definite
#'
#' Test whether a symmetric matrix is positive
#' definite, based on all its eigenvalues being
#' (close to) positive.
#'
#' Based on code in MASS::mvrnorm.
#'
#' Calculating eigenvalues is fast, so this
#' test is fast.
#'
#' @param A symmetric matrix
#' @param tolerance Numerical tolerance
#' for deciding whether eigenvalue positive.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_positive_definite <- function(m, tolerance = 1e-6) {
  eigen_values <- eigen(m, symmetric = TRUE, only.values = TRUE)$values
  lower_limit <- -1 * tolerance * abs(eigen_values[[1L]])
  all(eigen_values > lower_limit)
}


## HAS_TESTS
#' Calculate exposure from raw population data
#'
#' Calculate exposure from input data on population,
#' then align to events data.
#'
#' Population and events data are assumed to have
#' the same classification variables (though some different
#' levels).
#'
#' @param popn_df Data frame with population data.
#' Each measurement column is one dataset, with data
#' ordered by reliability: left-most columns are
#' most reliable.
#' @param events_df Data frame with events data.
#'
#' @returns A vector of exposures, ordered to line
#' up with 'events_df'.
#'
#' @noRd
make_exposure_direct <- function(popn_df, events_df) {
  nms_df <- names(popn_df)
  nms_classif_vars <- c("age", "sex", "cohort", "time")
  nms_classif_obs <- intersect(nms_classif_vars, nms_df)
  nms_classif_nocohort <- setdiff(nms_classif_obs, "cohort")
  has_age <- "age" %in% nms_classif_obs
  has_sex <- "sex" %in% nms_classif_obs
  has_cohort <- "cohort" %in% nms_classif_obs
  has_time <- "time" %in% nms_classif_obs
  has_lexis <- has_age && has_cohort && has_time
  popn <- get_best_value_one(
    df = popn_df,
    nm = "population"
  )
  nms_by <- NULL
  if (has_age) {
    nms_by <- c(nms_by, "age")
  }
  if (has_sex) {
    nms_by <- c(nms_by, "sex")
  }
  if (has_cohort && !has_age) {
    nms_by <- c(nms_by, "cohort")
  }
  if (is.null(by)) {
    popn <- list(popn)
  } else {
    popn <- split(popn, popn[nms_by])
  }
  exposure <- lapply(popn,
    make_exposure_direct_inner,
    has_lexis = has_lexis
  )
  exposure <- do.call(rbind, exposure)
  nms_classif_expose <- c(nms_by, "time")
  exposure <- exposure[c(nms_classif_expose, "exposure")]
  paste_dot <- function(...) paste(..., sep = ".")
  key_events <- do.call(paste_dot, events_df[nms_classif_expose])
  key_exposure <- do.call(paste_dot, exposure[nms_classif_expose])
  i <- match(key_events, key_exposure)
  ## return vector of exposures
  exposure$exposure[i]
}


## HAS_TESTS
#' Calculate exposure for one sub-population
#'
#' If Lexis triangles are being used,
#' divide result by two.
#'
#' @param df Data frame with
#' - population variable
#' - one or more of age, time, or cohort
#' - optional other variables, which are ignored
#' @param has_lexis Whether exposures
#' are to be used with Lexis triangles
#'
#' @returns A data frame with the original
#' variables, execept that the 'population'
#' variable is replaced with an 'exposure' variable.
#' There is also one less row.
#'
#' @noRd
make_exposure_direct_inner <- function(df, has_lexis) {
  nms_df <- names(df)
  if ("time" %in% nms_df) {
    ord <- order(df$time)
  } else if ("age" %in% nms_df) {
    ord <- order(df$age)
  } else {
    ord <- order(df$cohort)
  }
  df <- df[ord, , drop = FALSE]
  popn_start <- df$population[-nrow(df)]
  popn_end <- df$population[-1L]
  mult <- if (has_lexis) 0.25 else 0.5
  exposure <- mult * (popn_start + popn_end)
  ans <- df[-1L, , drop = FALSE]
  ans <- ans[-match("population", names(ans))]
  ans$exposure <- exposure
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Make a mean vector to use in the case where
#' 'nlminb' fails
#'
#' @param parameters Parameter list, created during
#' call to fit.comod
#'
#' @returns A named vector of 0s
#'
#' @noRd
make_fail_mean <- function(parameters) {
  lengths <- lengths(parameters)
  n <- sum(lengths)
  nms <- rep(names(parameters), times = lengths)
  ans <- rep(NA_real_, times = n)
  names(ans) <- nms
  ans
}


## HAS_TESTS
#' Make a var matrix to use in the case where
#' 'nlminb' fails
#'
#' @param parameters Parametes list, created during
#' call to fit.comod
#'
#' @returns A matrix of zeros, with dimnames
#'
#' @noRd
make_fail_var <- function(parameters) {
  lengths <- lengths(parameters)
  n <- sum(lengths)
  nms <- rep(names(parameters), times = lengths)
  ans <- matrix(0, nrow = n, ncol = n)
  rownames(ans) <- nms
  colnames(ans) <- nms
  ans
}


## HAS_TESTS
#' Make label columns for 'components_cohort_datamods'
#'
#' @param codatamods A list of objects of class 'dpmaccount_codatamod'
#' @param nm_series Name of the demographic series
#'
#' @returns A tibble
#'
#' @noRd
make_labels_codatamods <- function(codatamods, nm_series) {
  nms_data <- names(codatamods)
  nms_par <- lapply(codatamods, get_nms_par)
  n_par <- lengths(nms_par)
  nms_data <- rep(nms_data, times = n_par)
  nms_par <- unlist(nms_par, use.names = FALSE)
  tibble::tibble(
    series = nm_series,
    data = nms_data,
    par = nms_par
  )
}


## HAS_TESTS
#' Convert matrix to list of columns
#'
#' Split matrix column by column
#'
#' @param m A matrix
#'
#' @returns A list of length ncol(m),
#' each element of which is a column
#' of length nrow(m).
#'
#' @noRd
matrix_to_list_cols <- function(m) {
  ans <- apply(m, MARGIN = 2L, FUN = unname, simplify = FALSE)
  ans <- unname(ans)
  ans
}


## HAS_TESTS
#' Convert matrix to list of rows
#'
#' Split matrix row by row
#'
#' @param m A matrix
#'
#' @returns A list of length nrow(m),
#' each element of which is a row
#' of length ncol(m).
#'
#' @noRd
matrix_to_list_rows <- function(m) {
  ans <- apply(m, MARGIN = 1L, FUN = unname, simplify = FALSE)
  ans <- unname(ans)
  ans
}


## HAS_TESTS
#' Nest data within groups, formatting data as
#' data frames
#'
#' Columns named in 'nms_data' are turned into
#' miniature data frames, indexed by
#' the columns in 'nms_group'. The new column
#' containing the miniature data frames is called
#' 'nm_value'.
#'
#' Simplified version of tidyr::nest.
#'
#' @param df A data frame
#' @param nms_data Names of variables in 'df'.
#' @param nms_group Names of variables in 'df'.
#' @param nm_value Name for column holding
#' mini data frames.
#'
#' @returns A data frame
#'
#' @noRd
nest_to_df <- function(df, nms_data, nms_group, nm_value) {
  data <- df[nms_data]
  group <- df[nms_group]
  l <- split(data, group, lex.order = TRUE)
  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }
  l <- lapply(l, unrowname)
  split_to_df(
    l = l,
    nms_group = nms_group,
    nm_value = nm_value
  )
}


## HAS_TESTS
#' Nest data within groups, formatting data as
#' lists of vectors
#'
#' The column named in 'nm_data' is turned into
#' a list of vectors, indexed by
#' the columns in 'nms_group'. The new column
#' containing the list is called
#' 'nm_value'.
#'
#' Modified version of tidyr::nest.
#'
#' @param df A data frame
#' @param nm_data Name of variable in 'df'.
#' @param nm_id Name of variable distinguishing
#' elements of data, within each combination
#' of grouping variables.
#' @param nms_group Names of variables in 'df'.
#' @param nm_value Name for column holding
#' list of vectors.
#'
#' @returns A data frame
#'
#' @noRd
nest_to_list <- function(df, nm_data, nm_id, nms_group, nm_value) {
  data <- df[[nm_data]]
  id <- df[[nm_id]]
  group <- df[nms_group]
  names(data) <- id
  l <- split(data, group, lex.order = TRUE)
  split_to_df(
    l = l,
    nms_group = nms_group,
    nm_value = nm_value
  )
}


## HAS_TESTS
#' Generate stock, births, deaths, ins, outs for a single cohort
#'
#' @param is_new_cohort Whether the cohort was born during
#' the estimation period
#' @param prior_stk_init Prior for initial stock.
#' List with elements 'mean' and 'sd'.
#' @param sysmod_bth, sysmod_dth, sysmod_ins, sysmod_outs
#' System models for births, deaths, ins, and outs, each
#' a list with elements 'mean' and 'disp'.
#'
#' @returns A named list of vectors of doubles, with
#' elements 'val_stk', 'val_bth', 'val_dth',
#' 'val_ins', and 'val_outs'.
#'
#'
#' @noRd
project_cohort <- function(is_new_cohort,
                           prior_stk_init,
                           sysmod_bth,
                           sysmod_dth,
                           sysmod_ins,
                           sysmod_outs) {
  ## vectors to hold results
  K <- length(sysmod_dth$mean)
  val_stk <- numeric(length = K + 1L)
  val_dth <- numeric(length = K)
  val_ins <- numeric(length = K)
  val_outs <- numeric(length = K)
  ## initial value for stock
  mean_stk_init <- prior_stk_init$mean
  sd_stk_init <- prior_stk_init$sd
  if (is.finite(sd_stk_init)) {
    val_stk[[1L]] <- max(
      round(stats::rnorm(
        n = 1L,
        mean = mean_stk_init,
        sd = sd_stk_init
      )),
      0
    )
  } else {
    val_stk[[1L]] <- stats::rpois(
      n = 1L,
      lambda = mean_stk_init
    )
  }
  ## iterate through triangles
  for (k in seq_len(K)) {
    ## generate ins
    val_ins[[k]] <- with(
      sysmod_ins,
      stats::rnbinom(
        n = 1L,
        size = 1 / disp[[k]],
        mu = mean[[k]]
      )
    )
    ## half of the ins enter at the start, half at the end
    val_ins_start <- whole_num_halve(val_ins[[k]])
    val_ins_end <- val_ins[[k]] - val_ins_start
    stk_start <- val_stk[[k]] + val_ins_start
    ## generate rates for deaths and outs
    rate_dth <- with(
      sysmod_dth,
      stats::rgamma(
        n = 1L,
        shape = 1 / disp[[k]],
        rate = 1 / (disp[[k]] * mean[[k]])
      )
    )
    rate_outs <- with(
      sysmod_outs,
      stats::rgamma(
        n = 1L,
        shape = 1 / disp[[k]],
        rate = 1 / (disp[[k]] * mean[[k]])
      )
    )
    ## turn into multinomial draws of deaths and outs
    prob_exit <- 1 - exp(-0.5 * (rate_dth + rate_outs))
    val_exit <- stats::rbinom(
      n = 1L,
      size = stk_start,
      prob = prob_exit
    )
    prob_dth <- rate_dth / (rate_dth + rate_outs)
    val_dth[[k]] <- stats::rbinom(
      n = 1L,
      size = val_exit,
      prob = prob_dth
    )
    val_outs[[k]] <- val_exit - val_dth[[k]]
    ## apply demographic accounting equation to derive stock
    val_stk[[k + 1L]] <- val_stk[[k]] - val_exit + val_ins[[k]]
  }
  ## generate births
  if (is_new_cohort) {
    val_bth <- rep(list(c(Female = NA_real_, Male = NA_real_)),
      times = K
    )
  } else {
    exposure <- 0.25 * (val_stk[-(K + 1L)] + val_stk[-1L])
    val_bth_female <- with(
      sysmod_bth,
      stats::rnbinom(
        n = K,
        size = 1 / disp,
        mu = mean * exposure
      )
    )
    val_bth_male <- with(
      sysmod_bth,
      stats::rnbinom(
        n = K,
        size = 1 / disp,
        mu = mean * exposure
      )
    )
    val_bth <- lapply(
      1:K,
      function(k) {
        c(
          Female = val_bth_female[k],
          Male = val_bth_male[k]
        )
      }
    )
  }
  ## return results
  list(
    val_stk = val_stk, ## K + 1L
    val_bth = val_bth, ## K
    val_dth = val_dth, ## K
    val_ins = val_ins, ## K
    val_outs = val_outs
  ) ## K
}


## HAS_TESTS
#' Convert births data from sex-of-parent format
#' to sex-of-child format
#'
#' Given a data frame where the 'sex' variable
#' refers to the sex of the parent, and birth
#' counts consist of a list of vectors (with
#' one element for each sex), convert to the
#' format where the 'sex' variable refers to the
#' sex of the child.
#'
#' @param df A data frame
#' @param nm_bth The name of the variable
#' holding births counts
#'
#' @returns A data frame
#'
#' @seealso unpivot_sex_index_births for the approximate inverse
#'
#' @noRd
pivot_sex_index_births <- function(df, nm_bth) {
  nms_df <- names(df)
  if (!("sex" %in% nms_df)) {
    stop(
      gettextf(
        "data frame does not contain '%s' variable",
        "sex"
      ),
      call. = FALSE
    )
  }
  nms_classif_vars <- intersect(c("age", "sex", "cohort", "time"), nms_df)
  bth_parent <- df[[nm_bth]]
  bth_child <- unlist(bth_parent)
  has_non_sex_classif <- length(nms_classif_vars) > 1L
  if (has_non_sex_classif) {
    i_row <- rep(seq_along(bth_parent), times = vapply(bth_parent, length, 0L))
    df_child <- df[i_row, setdiff(nms_classif_vars, "sex"), drop = FALSE]
    df_child$sex <- names(bth_child)
    df_child$births <- unname(bth_child)
    paste_dot <- function(...) paste(..., sep = ".")
    id_parent <- do.call(paste_dot, df[nms_classif_vars])
    id_child <- do.call(paste_dot, df_child[nms_classif_vars])
  } else {
    df_child <- data.frame(
      sex = names(bth_child),
      births = unname(bth_child)
    )
    id_parent <- df$sex
    id_child <- df_child$sex
  }
  df[[nm_bth]] <- NA_real_
  df[[nm_bth]] <- df_child$births[match(id_parent, id_child)]
  df
}



## HAS_TESTS
#' Simulate time and age variables
#' for a cohort
#'
#' @param K Number of triangles
#' @param is_new_cohort Whether cohort born during
#' estimation period
#'
#' @returns A data frame with variables
#' 'time' and 'age'
#'
#' @noRd
sim_time_age <- function(K, cohort, is_new_cohort) {
  if (is_new_cohort) {
    s_time <- seq.int(from = cohort, length.out = (K + 1L) / 2L)
    time <- rep(s_time, each = 2L)[2L:(K + 1L)]
    triangle <- rep(0:1, times = ceiling((K + 1L) / 2L))[1L:K]
  } else {
    age_start <- 30L
    s_time <- seq.int(
      from = cohort + age_start + 1L,
      length.out = (K + 1L) / 2L
    )
    time <- rep(s_time, each = 2L)[1L:K]
    triangle <- rep(0:1, times = ceiling((K + 1L) / 2L))[2L:(K + 1L)]
  }
  age <- time - cohort - triangle
  data.frame(
    time = time,
    age = age
  )
}


## HAS_TESTS
#' Convert the results from function 'split' into
#' a data frame with a list column
#'
#' Includes converting 'age', 'cohort', and 'time'
#' columns to integer.
#'
#' @param l Results from a call to function 'split'.
#' @param nms_groups The names of the variables used
#' as the 'f' argument in 'split'
#' @param nm_value The name of the list column.
#'
#' @returns A tibble
#'
#' @noRd
split_to_df <- function(l, nms_group, nm_value) {
  ans <- strsplit(names(l), split = ".", fixed = TRUE)
  ans <- do.call(rbind, ans)
  ans <- data.frame(ans)
  ans <- tibble::tibble(ans)
  colnames(ans) <- nms_group
  is_int <- nms_group %in% c("age", "cohort", "time")
  ans[is_int] <- lapply(ans[is_int], as.integer)
  ans[[nm_value]] <- unname(l)
  ans
}


## HAS_TESTS
#' Summarise data from a list of codatamods
#'
#' @param mod A list of objects of class "dpmaccount_codatamod"
#'
#' @returns A tibble, or NULL if 'mods' has length 0
#'
#' @noRd
summarise_codatamods <- function(mods) {
  n <- length(mods)
  if (n == 0L) {
    return(NULL)
  }
  nms <- names(mods)
  ans <- .mapply(get_data_cols,
    dots = list(x = mods, nm = nms),
    MoreArgs = list()
  )
  ans <- Reduce(full_join_timeage, ans)
  ans
}


## HAS_TESTS
#' Construct means of ins, outs, and population
#' from output from TMB
#'
#' @param mean Named numeric vector produced by TMB
#' @param prior_stk_init Named list
#'
#' @returns A tibble
#'
#' @noRd
summarise_estimated_means <- function(mean, prior_stk_init) {
  nms <- names(mean)
  is_stk_init_estimated <- "log_val_stk_init" %in% nms
  if (is_stk_init_estimated) {
    stk_init <- exp(mean[nms == "log_val_stk_init"])
  } else {
    stk_init <- prior_stk_init$mean
  }
  mean_ins <- exp(mean[nms == "log_val_ins"])
  mean_outs <- exp(mean[nms == "log_val_outs"])
  mean_stk_end <- stk_init + cumsum(mean_ins) - cumsum(mean_outs)
  data.frame(
    mean_stk_end = unname(mean_stk_end),
    mean_ins = unname(mean_ins),
    mean_outs = unname(mean_outs)
  )
}


## HAS_TESTS
#' Summarise data from a list of system models
#'
#' @param mod A list of data frames.
#'
#' @returns A tibble, or NULL if 'mods' has length 0
#'
#' @noRd
summarise_sysmods <- function(mods) {
  get_cols <- function(x, nm) {
    ans <- x[c("time", "age", "mean")]
    names(ans)[[3L]] <- nm
    ans
  }
  nms <- names(mods)
  ans <- .mapply(get_cols,
    dots = list(x = mods, nm = nms),
    MoreArgs = list()
  )
  ans <- Reduce(full_join_timeage, ans)
  ans
}


## HAS_TESTS
#' Convert births data from sex-of-child format
#' to sex-of-parent format
#'
#' Given a data frame where the 'sex' variable
#' refers to the sex of the child, and birth
#' counts are numeric vectors, convert to the
#' format where the 'sex' variable refers to the
#' sex of the parent (assumed to be "Female"),
#' and where the sex of births is not recorded.
#'
#' @param df A data frame
#' @param nm_bth The name of the variable
#' holding births counts
#'
#' @returns A data frame
#'
#' @seealso pivot_sex_index_births for the
#' approximate inverse
#'
#' @noRd
unpivot_sex_index_births <- function(df, nm_bth) {
  nms_classif_vars <- c("age", "sex", "cohort", "time")
  nms_df <- names(df)
  if (!("sex" %in% nms_df)) {
    stop(
      gettextf(
        "data frame does not contain '%s' variable",
        "sex"
      ),
      call. = FALSE
    )
  }
  nms_classif_obs <- intersect(nms_classif_vars, nms_df)
  nms_classif_nosex <- setdiff(nms_classif_obs, "sex")
  df_ag <- stats::aggregate(
    df[nm_bth],
    df[nms_classif_nosex],
    sum
  )
  df_ag$sex <- "Female"
  df[[nm_bth]] <- NA_real_
  paste_dot <- function(...) paste(..., sep = ".")
  key_df <- do.call(paste_dot, df[nms_classif_obs])
  key_df_ag <- do.call(paste_dot, df_ag[nms_classif_obs])
  i_ag <- match(key_df, key_df_ag)
  df[[nm_bth]] <- df_ag[[nm_bth]][i_ag]
  df
}


## HAS_TESTS
#' Halve a number, and then if the result is not
#' a whole number, randomly round to whole number
#'
#' @param x Number to halve
#'
#' @returns A integerish double
#'
#' @noRd
whole_num_halve <- function(x) {
  ans <- x / 2
  remainder <- ans - floor(ans)
  if (isTRUE(all.equal(remainder, 0))) {
    ans
  } else {
    add <- stats::rbinom(n = 1L, size = 1L, prob = remainder)
    floor(ans) + add
  }
}
