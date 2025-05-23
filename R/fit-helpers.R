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
#' @param parameters Parameter list, created during
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
#' Make 'map' argument to MakeADFun
#'
#' Make the 'map' argument to TMB::MakeADFun.
#' The 'map' argument can be used to fix
#' parameters at their initial value. At present
#' we only do this 'log_val_stk_init', ie
#' the log of the initial stock, which we
#' fix when its prior standard deviation is 0.
#' (Currently this happens when the initial
#' stock consists of births.)
#'
#' @param prior_stk_init List specifying prior
#' for initial stock.
#'
#' @returns NULL or a named list.
#'
#' @noRd
make_map <- function(prior_stk_init) {
  sd <- prior_stk_init$sd
  if (sd > 0) {
    NULL
  } else {
    list(log_val_stk_init = factor(NA))
  }
}


## HAS_TESTS
#' Make 'parameters' argument to MakeADFun
#'
#' Make the 'parameters' argument to TMB::MakeADFun.
#' The 'parameters' argument holds the
#' initial values for parameters, and also tells
#' TMB the names and size of parameters.
#' defines the lengths/dimensions of the parameters.
#'
#' We increase the initial value for population
#' to a higher level than is suggested
#' by the data. There appears to be an asymmetry,
#' where TMB can cope with an initial population
#' value that is (much) too high, but cannot
#' cope with one that is too low.
#'
#' @param mean_stk_init Mean in prior for
#' for initial stock.
#' @param sd_stk_init Standard deviation in
#' prior for initial stock
#' @param val_dth Counts of deaths
#' @param means_ins Expected counts from
#' system model for ins
#' @param mean_outs Expected rates from
#' system model for outs
#' @param datamods_stk List of objects of
#' class "dpmaccount_codatamod", containing
#' data and data models for population.
#' @param datamods_ins List of objects of
#' class "dpmaccount_codatamod", containing
#' data on and data models for ins.
#' @param datamods_outs List of objects of
#' class "dpmaccount_codatamod", containing
#' data on and data models for outs.
#'
#' @returns NULL or a named list.
#'
#' @noRd
make_parameters <- function(mean_stk_init,
                            sd_stk_init,
                            val_dth,
                            mean_ins,
                            mean_outs,
                            datamods_stk,
                            datamods_ins,
                            datamods_outs) {
  vals_init <- make_vals_init(
    mean_stk_init = mean_stk_init,
    sd_stk_init = sd_stk_init,
    val_dth = val_dth,
    mean_ins = mean_ins,
    mean_outs = mean_outs
  )
  par_all_stk <- get_par_all(datamods_stk)
  par_all_ins <- get_par_all(datamods_ins)
  par_all_outs <- get_par_all(datamods_outs)
  list(
    log_val_stk_init = vals_init$log_val_stk_init,
    log_val_ins = vals_init$log_val_ins,
    log_val_outs = vals_init$log_val_outs,
    par_all_stk = par_all_stk,
    par_all_ins = par_all_ins,
    par_all_outs = par_all_outs
  )
}


## HAS_TESTS
#' Construct initial values for
#' log_val_ins, and log_val_outs,
#' and possibly val_stk_init
#'
#' All values of val_ins and val_outs,
#' plus derived values for stock,
#' guaranteed to be greater than or equal to 'eps'.
#'
#' If 'val_stk_init' is being treated as uncertain,
#' then it can be different from prior mean.
#' If 'val_stk_init' is being treated as certain
#' prior mean is 0, then set 'val_stk_init' to 'eps'
#' (to avoid convergence problems.)
#'
#' @param mean_stk_init,sd_stk_init
#' Prior for initial stock.
#' @param mean_ins Expected values for val_ins,
#' from system model
#' @param mean_outs Expected values for rate
#' for outs, from system model
#' @param val_dth Death counts
#'
#' @returns A named list
#'
#' @noRd
make_vals_init <- function(mean_stk_init,
                           sd_stk_init,
                           val_dth,
                           mean_ins,
                           mean_outs) {
  max_iter <- 10000
  eps <- 1e-6
  is_stk_init_nonzero <- mean_stk_init > 0
  is_stk_init_estimated <- sd_stk_init > 0
  succeeded <- FALSE
  K <- length(val_dth)
  for (i in seq.int(from = 0L, to = max_iter)) {
    if (is_stk_init_estimated) {
      val_stk_init <- exp(0.01 * i) * mean_stk_init
      val_stk_init <- max(val_stk_init, eps)
    } else {
      if (is_stk_init_nonzero) {
        val_stk_init <- mean_stk_init
      } else {
        val_stk_init <- eps
      }
    }
    val_ins <- exp(0.01 * i) * mean_ins
    val_ins <- pmax(val_ins, eps)
    rate_outs <- exp(-0.01 * i) * mean_outs
    val_outs <- double(length = K)
    stk <- val_stk_init
    for (k in seq_len(K)) {
      stk <- stk + val_ins[k] - val_dth[k]
      if (stk < eps) {
        break
      }
      exposure <- 0.5 * stk ## 0.5 because Lexis triangle
      val_outs[k] <- mean_outs[k] * exposure
      val_outs[k] <- max(val_outs[k], eps)
      stk <- stk - val_outs[k]
      if (stk < eps) {
        break
      }
      if (k == K) {
        succeeded <- TRUE
      }
    }
    if (succeeded) {
      break
    }
  }
  if (!succeeded) {
    stop("Internal error: Unable to generate initial values.",
      call. = FALSE
    )
  }
  list(
    log_val_stk_init = log(val_stk_init),
    log_val_ins = log(val_ins),
    log_val_outs = log(val_outs)
  )
}
