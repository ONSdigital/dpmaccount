## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293



## 'get_const' -------------------------------------------------------------------

#' Get parameters not estimated by TMB
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A vector of doubles
#'
#' @noRd
get_const <- function(x) {
  UseMethod("get_const")
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_norm_haspar <- function(x) {
  c(x$ratio, x$sd, x$scale_ratio, x$scale_sd)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_norm_nopar <- function(x) {
  c(x$ratio, x$sd)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_t_haspar <- function(x) {
  c(x$ratio, x$scale, x$df, x$scale_ratio)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_t_nopar <- function(x) {
  c(x$ratio, x$scale, x$df)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_nbinom_haspar <- function(x) {
  c(x$ratio, x$disp, x$scale_ratio)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_nbinom_nopar <- function(x) {
  c(x$ratio, x$disp)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_poisson_haspar <- function(x) {
  c(x$ratio, x$scale_ratio)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_poisson_nopar <- function(x) {
  c(x$ratio)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_lognorm_haspar <- function(x) {
  c(x$ratio, x$sd, x$scale_ratio)
}

## HAS_TESTS
#' @export
get_const.dpmaccount_codatamod_lognorm_nopar <- function(x) {
  c(x$ratio, x$sd)
}


## 'get_data' -----------------------------------------------------------------

#' Get observed data values supplied through data models
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A vectors of doubles
#'
#' @noRd
get_data <- function(x) {
  UseMethod("get_data")
}

## HAS_TESTS
#' @export
get_data.dpmaccount_codatamod <- function(x) {
  as.double(x$data)
}


## 'get_data_cols' ------------------------------------------------------------

#' Get reported values, plus time and age
#'
#' @param x An object of class 'dpmaccount_codatamod'
#' @param nm The name of the dataset
#'
#' @returns A data frame
#'
#' @noRd
get_data_cols <- function(x, nm) {
  UseMethod("get_data_cols")
}

## HAS_TESTS
#' @export
get_data_cols.dpmaccount_codatamod_norm <- function(x, nm) {
  ans <- x[c("time", "age", "data")]
  ans <- tibble::as_tibble(ans)
  names(ans)[[3L]] <- nm
  ans
}

## HAS_TESTS
#' @export
get_data_cols.dpmaccount_codatamod_t <- function(x, nm) {
  ans <- x[c("time", "age", "data")]
  ans <- tibble::as_tibble(ans)
  names(ans)[[3L]] <- nm
  ans
}

## HAS_TESTS
#' @export
get_data_cols.dpmaccount_codatamod_nbinom <- function(x, nm) {
  ans <- x[c("time", "age", "data")]
  ans <- tibble::as_tibble(ans)
  names(ans)[[3L]] <- nm
  ans
}

## HAS_TESTS
#' @export
get_data_cols.dpmaccount_codatamod_poisson <- function(x, nm) {
  ans <- x[c("time", "age", "data")]
  ans <- tibble::as_tibble(ans)
  names(ans)[[3L]] <- nm
  ans
}

## HAS_TESTS
#' @export
get_data_cols.dpmaccount_codatamod_lognorm <- function(x, nm) {
  ans <- x[c("time", "age", "data")]
  ans <- tibble::as_tibble(ans)
  names(ans)[[3L]] <- nm
  ans
}

## 'get_has_par' --------------------------------------------------------------

#' Get flag recording whether model has parameters
#' estimated by TMB
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A single integer.
#'
#' @noRd
get_has_par <- function(x) {
  UseMethod("get_has_par")
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_norm_haspar <- function(x) {
  TRUE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_norm_nopar <- function(x) {
  FALSE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_t_haspar <- function(x) {
  TRUE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_t_nopar <- function(x) {
  FALSE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_nbinom_haspar <- function(x) {
  TRUE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_nbinom_nopar <- function(x) {
  FALSE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_poisson_haspar <- function(x) {
  TRUE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_poisson_nopar <- function(x) {
  FALSE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_lognorm_haspar <- function(x) {
  TRUE
}

## HAS_TESTS
#' @export
get_has_par.dpmaccount_codatamod_lognorm_nopar <- function(x) {
  FALSE
}


## 'get_i_mod' ----------------------------------------------------------------

#' Get index for model
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A single integer.
#'
#' @noRd
get_i_mod <- function(x) {
  UseMethod("get_i_mod")
}

## HAS_TESTS
#' @export
get_i_mod.dpmaccount_codatamod <- function(x) {
  x$i_mod
}


## 'get_is_obs' ---------------------------------------------------------------

#' Get indicator for whether dataset contains observation
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A vector of integers.
#'
#' @noRd
get_is_obs <- function(x) {
  UseMethod("get_is_obs")
}

## HAS_TESTS
#' @export
get_is_obs.dpmaccount_codatamod <- function(x) {
  x$is_obs
}


## 'get_par' -------------------------------------------------------------------

#' Get parameters estimated by TMB
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A vector of doubles
#'
#' @noRd
get_par <- function(x) {
  UseMethod("get_par")
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_norm_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  has_mult_sd <- x$scale_sd > 0
  rep(0, times = has_mult_ratio + has_mult_sd)
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_norm_nopar <- function(x) {
  double()
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_t_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(0, times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_t_nopar <- function(x) {
  double()
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_nbinom_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(0, times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_nbinom_nopar <- function(x) {
  double()
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_poisson_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(0, times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_poisson_nopar <- function(x) {
  double()
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_lognorm_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(0, times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_par.dpmaccount_codatamod_lognorm_nopar <- function(x) {
  double()
}


## 'get_nms_par' --------------------------------------------------------------

#' Get names of parameters estimated by TMB
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A character vector
#'
#' @noRd
get_nms_par <- function(x) {
  UseMethod("get_nms_par")
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_norm_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  has_mult_sd <- x$scale_sd > 0
  ans <- character()
  if (has_mult_ratio) {
    ans <- c(ans, "mult_ratio")
  }
  if (has_mult_sd) {
    ans <- c(ans, "mult_sd")
  }
  ans
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_norm_nopar <- function(x) {
  character()
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_t_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  ans <- character()
  if (has_mult_ratio) {
    ans <- c(ans, "mult_ratio")
  }
  ans
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_t_nopar <- function(x) {
  character()
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_nbinom_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  ans <- character()
  if (has_mult_ratio) {
    ans <- c(ans, "mult_ratio")
  }
  ans
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_nbinom_nopar <- function(x) {
  character()
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_poisson_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  ans <- character()
  if (has_mult_ratio) {
    ans <- c(ans, "mult_ratio")
  }
  ans
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_poisson_nopar <- function(x) {
  character()
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_lognorm_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  ans <- character()
  if (has_mult_ratio) {
    ans <- c(ans, "mult_ratio")
  }
  ans
}

## HAS_TESTS
#' @export
get_nms_par.dpmaccount_codatamod_lognorm_nopar <- function(x) {
  character()
}

## 'get_transform_datamod' ----------------------------------------------------

#' Get transforms to apply to parameters estimated by TMB
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns A list
#'
#' @noRd
get_transform_datamod <- function(x) {
  UseMethod("get_transform_datamod")
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_norm_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  has_mult_sd <- x$scale_sd > 0
  rep(list(identity), times = has_mult_ratio + has_mult_sd)
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_norm_nopar <- function(x) {
  list()
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_t_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(list(identity), times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_t_nopar <- function(x) {
  list()
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_nbinom_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(list(identity), times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_nbinom_nopar <- function(x) {
  list()
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_poisson_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(list(identity), times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_poisson_nopar <- function(x) {
  list()
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_lognorm_haspar <- function(x) {
  has_mult_ratio <- x$scale_ratio > 0
  rep(list(identity), times = has_mult_ratio)
}

## HAS_TESTS
#' @export
get_transform_datamod.dpmaccount_codatamod_lognorm_nopar <- function(x) {
  list()
}

## 'increments_codatamod' -----------------------------------------------------

#' Calculate increments within Lexis triangles
#'
#' Take time and age information and calculates corresponding cohort and Lexis triangle indexing
#'
#' @param x Object of class "dpmaccount_codatamod"
#' @param cohort Numeric - cohort that data refers to
#' @param nm_data Name of the dataset
#' @param is_stock Whether the object holds stock data
#'
#' @returns A tibble
#'
#' @export
increments_codatamod <- function(x, cohort, nm_data, is_stock) {
  UseMethod("increments_codatamod")
}

## HAS_TESTS
#' @export
increments_codatamod.dpmaccount_codatamod <- function(x, cohort, nm_data, is_stock) {
  time <- x$time
  age <- x$age
  data <- x$data
  if (is_stock) {
    is_popn <- age == time - cohort
    if (sum(is_popn) > 1L) {
      time <- time[is_popn][-1L]
      data <- diff(data[is_popn])
    } else {
      time <- integer()
      data <- double()
    }
  } else {
    is_new_cohort <- time[[1L]] == cohort
    is_upper <- age == time - cohort - 1L
    is_extinct_cohort <- is_upper[length(time)]
    if (is_new_cohort) {
      time <- time[-1L]
      data <- data[-1L]
    }
    if (is_extinct_cohort) {
      time <- time[-length(time)]
      data <- data[-length(data)]
    }
    if (length(time) >= 2L) {
      time <- time[c(TRUE, FALSE)]
      idx <- rep(seq_along(time), each = 2L)
      data <- tapply(data, idx, sum)
      data <- as.double(data)
    } else {
      time <- integer()
      data <- double()
    }
  }
  ans <- tibble::tibble(
    time = time,
    data = data
  )
  names(ans)[[2L]] <- nm_data
  ans
}


## 'make_str' -----------------------------------------------------------------

#' Make string to be used in print.dpmaccount_results
#'
#' @param x An object of class 'dpmaccount_codatamod'
#' @param nm_data Name of dataset
#' @param nm_series Name of demographic series
#'
#' @returns A string
#'
#' @noRd
make_str <- function(x, nm_data, nm_series) {
  UseMethod("make_str")
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_norm_haspar <- function(x, nm_data, nm_series) {
  has_scale_ratio <- x$scale_ratio > 0
  has_scale_sd <- x$scale_sd > 0
  if (has_scale_ratio && has_scale_sd) {
    sprintf("%12s ~ N(exp(alpha) * ratio * %s, (exp(beta) * sd)^2)\n", nm_data, nm_series)
  } else if (has_scale_ratio && !has_scale_sd) {
    sprintf("%12s ~ N(exp(alpha) * ratio * %s, sd^2)\n", nm_data, nm_series)
  } else {
    sprintf("%12s ~ N(ratio * %s, (exp(beta) * sd)^2)\n", nm_data, nm_series)
  }
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_norm_nopar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ N(ratio * %s, sd^2)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_t_haspar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ t(df, exp(alpha) * ratio * %s, scale^2)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_t_nopar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ t(df, ratio * %s, scale^2)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_nbinom_haspar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ nbinom(exp(alpha) * ratio * %s, disp)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_nbinom_nopar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ nbinom(ratio * %s, disp)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_poisson_haspar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ poisson(exp(alpha) * ratio * %s)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_poisson_nopar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ poisson(ratio * %s)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_lognorm_haspar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ LN(exp(alpha) * ratio * %s, sd^2)\n", nm_data, nm_series)
}

## HAS_TESTS
#' @export
make_str.dpmaccount_codatamod_lognorm_nopar <- function(x, nm_data, nm_series) {
  sprintf("%12s ~ LN(ratio * %s, sd^2)\n", nm_data, nm_series)
}


## 'print' --------------------------------------------------------------------

#' @export
print.dpmaccount_codatamod_norm_haspar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    sd = x$sd,
    time = x$time,
    age = x$age
  )
  cat("Normal cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n")
  cat("scale_ratio:", x$scale_ratio, "\n\n")
  cat("scale_sd:", x$scale_sd, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_norm_nopar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    sd = x$sd,
    time = x$time,
    age = x$age
  )
  cat("Normal cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  print(df)
  invisible(x)
}


#' @export
print.dpmaccount_codatamod_t_haspar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    df = x$df,
    ratio = x$ratio,
    scale = x$scale,
    time = x$time,
    age = x$age
  )
  cat("t-distribution cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  cat("scale_ratio:", x$scale_ratio, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_t_nopar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    df = x$df,
    ratio = x$ratio,
    scale = x$scale,
    time = x$time,
    age = x$age
  )
  cat("t-distribution cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_nbinom_haspar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    disp = x$disp,
    time = x$time,
    age = x$age
  )
  cat("Negative-binomial-distribution cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  cat("scale_ratio:", x$scale_ratio, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_nbinom_nopar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    disp = x$disp,
    time = x$time,
    age = x$age
  )
  cat("Negative-binomial-distribution cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_poisson_haspar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    time = x$time,
    age = x$age
  )
  cat("Poisson-distribution cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  cat("scale_ratio:", x$scale_ratio, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_poisson_nopar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    time = x$time,
    age = x$age
  )
  cat("Poisson-distribution cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_lognorm_haspar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    sd = x$sd,
    time = x$time,
    age = x$age
  )
  cat("Log-Normal cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n")
  cat("scale_ratio:", x$scale_ratio, "\n\n")
  print(df)
  invisible(x)
}

#' @export
print.dpmaccount_codatamod_lognorm_nopar <- function(x, ...) {
  df <- data.frame(
    data = x$data,
    is_obs = x$is_obs,
    ratio = x$ratio,
    sd = x$sd,
    time = x$time,
    age = x$age
  )
  cat("Log-Normal cohort data model : An object of class \"", class(x)[[1L]], "\"\n\n", sep = "")
  cat("i_mod:", x$i_mod, "\n\n")
  print(df)
  invisible(x)
}




## 'rgen' -------------------------------------------------------------------

#' Generate draws from the data model
#'
#' @param x An object of class 'dpmaccount_codatamod'
#'
#' @returns An object of class 'dpmaccount_codatamod'
#'
#' @noRd
rgen <- function(x) {
  UseMethod("rgen")
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_norm_haspar <- function(x) {
  # check that this is the right interpretation of the data model
  alpha_c <- stats::rnorm(length(x$data), mean = 0, sd = x$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)
  beta_c <- stats::rnorm(length(x$data), mean = 0, sd = x$scale_sd) #  β_c \sim \mathcal{N}(0, A_{β}^2)
  gen_sd <- exp(beta_c) * x$sd
  gen_mean <- exp(alpha_c) * x$ratio * x$data
  new_data <- stats::rnorm(length(x$data), mean = gen_mean, sd = gen_sd) # y_i \sim \mathcal{N}(e^{α_{c_i}} ρ_i x_i, (e^{β_{c_i}} σ_i)^2)
  new_data <- ifelse(new_data > 0, new_data, 0)
  x$data <- new_data
  return(x)
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_norm_nopar <- function(x) {
  new_data <- stats::rnorm(length(x$data), mean = x$ratio * x$data, sd = x$sd)
  new_data <- ifelse(new_data > 0, new_data, 0)
  x$data <- new_data
  return(x)
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_t_haspar <- function(x) {
  alpha_c <- stats::rnorm(length(x$data), mean = 0, sd = x$scale_ratio) # α_c \sim \mathcal{N}(0, A_{α}^2)
  new_data <- stats::rt(length(x$data), df = x$df) * x$scale + x$data * x$ratio * exp(alpha_c)
  new_data <- ifelse(new_data > 0, new_data, 0)
  x$data <- new_data
  return(x)
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_t_nopar <- function(x) {
  new_data <- stats::rt(length(x$data), df = x$df) * x$scale + x$data * x$ratio
  new_data <- ifelse(new_data > 0, new_data, 0)
  x$data <- new_data
  return(x)
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_nbinom <- function(x) {
  new_data <- stats::rnbinom(length(x$data), mu = x$ratio * x$data, size = 1 / x$disp)
  x$data <- new_data
  return(x)
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_poisson <- function(x) {
  new_data <- stats::rpois(length(x$data), lambda = x$ratio * x$data)
  x$data <- new_data
  return(x)
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_lognorm_haspar <- function(x) {
  # check that this is the right interpretation of the data model
  alpha_c <- stats::rnorm(length(x$data), mean = 0, sd = x$scale_ratio)
  mean_orig <- exp(alpha_c) * x$ratio * x$data
  sd_orig <- x$sd
  mu <- log(mean_orig^2 / sqrt(sd_orig^2 + mean_orig^2))
  sd <- sqrt(log(1 + (sd_orig^2 / mean_orig^2)))
  new_data <- stats::rlnorm(length(x$data), mean = mu, sd = sd)
  new_data <- ifelse(new_data > 0, new_data, 0)
  x$data <- new_data
  return(x)
}

## NO_TESTS
#' @export
rgen.dpmaccount_codatamod_lognorm_nopar <- function(x) {
  mean_orig <- x$ratio * x$data
  sd_orig <- x$sd
  mu <- log(mean_orig^2 / sqrt(sd_orig^2 + mean_orig^2))
  sd <- sqrt(log(1 + (sd_orig^2 / mean_orig^2)))
  new_data <- stats::rlnorm(length(x$data), mean = mu, sd = sd)
  new_data <- ifelse(new_data > 0, new_data, 0)
  x$data <- new_data
  return(x)
}
