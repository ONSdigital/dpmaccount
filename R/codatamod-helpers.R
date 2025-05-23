## HAS_TESTS
#' Get combined constants from all cohort data models
#'
#' @param mods A list of data models
#'
#' @returns A vector of doubles.
#'
#' @noRd
get_const_all <- function(mods) {
  if (length(mods) > 0L) {
    ans <- lapply(mods, get_const)
    unlist(ans, use.names = FALSE)
  } else {
    double()
  }
}


## HAS_TESTS
#' Get combined data from all cohort data models
#'
#' @param mods A list of data models
#'
#' @returns A vector of doubles.
#'
#' @noRd
get_data_all <- function(mods) {
  if (length(mods) > 0L) {
    ans <- lapply(mods, get_data)
    unlist(ans, use.names = FALSE)
  } else {
    double()
  }
}


## HAS_TESTS
#' Get flags recording if data models have parameters
#'
#' @param mods A list of data models
#'
#' @returns A logical vector
#'
#' @noRd
get_has_par_all <- function(mods) {
  vapply(mods, get_has_par, FALSE)
}


## HAS_TESTS
#' Get factor to be used to distinguish constants
#'
#' @param mods A list of data models
#'
#' @returns An integer vector
#'
#' @noRd
get_i_mod_all <- function(mods) {
  vapply(mods, get_i_mod, 0L)
}


## HAS_TESTS
#' Get factor to be used to distinguish constants
#'
#' @param mods A list of data models
#'
#' @returns A factor
#'
#' @noRd
get_idx_const_all <- function(mods) {
  const <- lapply(mods, get_const)
  length_const <- vapply(const, length, 0L)
  s <- seq_along(length_const)
  ans <- rep(s, times = length_const)
  ans <- factor(ans, levels = s)
  ans
}


## HAS_TESTS
#' Get factor to be used to distinguish
#' datasets in combined data prior estimation by TMB
#'
#' @param mods A list of data models
#'
#' @returns A factor
#'
#' @noRd
get_idx_data_all <- function(mods) {
  data <- lapply(mods, get_data)
  length_data <- vapply(data, length, 0L)
  s <- seq_along(length_data)
  ans <- rep(s, times = length_data)
  ans <- factor(ans, levels = s)
  ans
}


## HAS_TESTS
#' Get factor to be used to distinguish
#' parameters prior to being estimated by TMB
#'
#' @param mods A list of data models
#'
#' @returns A factor
#'
#' @noRd
get_idx_par_all <- function(mods) {
  par <- lapply(mods, get_par)
  length_par <- vapply(par, length, 0L)
  s <- seq_along(length_par)
  ans <- rep(s, times = length_par)
  ans <- factor(ans, levels = s)
  ans
}


## HAS_TESTS
#' Get combined indicator used to specify
#' where a dataset has observations
#'
#' @param mods A list of data models.
#'
#' @returns A vector of integers.
#'
#' @noRd
get_is_obs_all <- function(mods) {
  if (length(mods) > 0L) {
    ans <- lapply(mods, get_is_obs)
    unlist(ans, use.names = FALSE)
  } else {
    integer()
  }
}


## HAS_TESTS
#' Get combined parameters prior to being estimated by
#' TMB from all cohort data models
#'
#' @param mods A list of data models.
#'
#' @returns A vector of doubles.
#'
#' @noRd
get_par_all <- function(mods) {
  if (length(mods) > 0L) {
    ans <- lapply(mods, get_par)
    unlist(ans, use.names = FALSE)
  } else {
    double()
  }
}


## HAS_TESTS
#' Get transformed data models prior to being estimated by
#' TMB from all cohort data models
#'
#' @param mods A list of data models.
#'
#' @returns A list of functions.
#'
#' @noRd
get_transform_datamod_all <- function(mods) {
  if (length(mods) > 0L) {
    ans <- lapply(mods, get_transform_datamod)
    unlist(ans, use.names = FALSE)
  } else {
    list()
  }
}
