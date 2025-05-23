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
#' Aggregate rate measure variables, while dropping one or more
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
#' @param seed_in The seed if reproducibility is required
#'
#' @returns A named list
#'
#' @noRd
draw_counts <- function(n_draw, mean, var, stk_init = NULL, val_dth,
                        cohort, sex, seed_in = NULL) {
  if (!is.null(seed_in)) {
    set.seed(seed_in)
  }
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
#' @param seed_in The seed if reproducibility is required
#'
#' @returns A matrix
#'
#' @noRd
draw_par_datamods <- function(object, population, events, seed_in = NULL) {
  if (!is.null(seed_in)) {
    set.seed(seed = seed_in)
  }
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
#' @param seed_in The seed if reproducibility is required
#'
#' @returns A list of vectors.
#'
#' @noRd
draw_rates <- function(sysmod, events, exposure, n_draw, seed_in = NULL) {
  if (!is.null(seed_in)) {
    set.seed(seed = seed_in)
  }
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
