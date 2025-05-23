## 'new' ---------------------------------------------------------------------

## HAS_TESTS
#' Create a new object of class "dpmaccount_codatamod_norm"
#'
#' @param args Arguments to the function.
#'
#' @return Object of class "dpmaccount_codatamod_norm"
#'
#' @noRd
new_codatamod_norm <- function(args) {
  scale_ratio <- args$scale_ratio[[1L]] ## values all same - take first
  scale_sd <- args$scale_sd[[1L]] ## values all same - take first
  if ((scale_ratio > 0) || (scale_sd > 0)) {
    ans <- list(
      i_mod = 101L,
      data = args$data,
      is_obs = args$is_obs,
      ratio = args$ratio,
      sd = args$sd,
      scale_ratio = scale_ratio,
      scale_sd = scale_sd,
      time = args$time,
      age = args$age
    )
    class(ans) <- c(
      "dpmaccount_codatamod_norm_haspar",
      "dpmaccount_codatamod_norm",
      "dpmaccount_codatamod"
    )
  } else {
    ans <- list(
      i_mod = 1L,
      data = args$data,
      is_obs = args$is_obs,
      ratio = args$ratio,
      sd = args$sd,
      time = args$time,
      age = args$age
    )
    class(ans) <- c(
      "dpmaccount_codatamod_norm_nopar",
      "dpmaccount_codatamod_norm",
      "dpmaccount_codatamod"
    )
  }
  ans
}

## HAS_TESTS
#' Create a new object of class "dpmaccount_codatamod_t"
#'
#' @param args Arguments to the function.
#'
#' @return Object of class "dpmaccount_codatamod_t"
#'
#' @noRd
new_codatamod_t <- function(args) {
  ans <- list(
    i_mod = 2L,
    data = args$data,
    is_obs = args$is_obs,
    df = args$df[[1L]], ## values all same - take first
    ratio = args$ratio,
    scale = args$scale,
    time = args$time,
    age = args$age
  )
  class(ans) <- c("dpmaccount_codatamod_t", "dpmaccount_codatamod")
  ans
}


## 'validate' -----------------------------------------------------------------

## HAS_TESTS
#' Checks shared across all object of class "dpmaccount_codatamod"
#'
#' @param x Object of class "dpmaccount_codatamod"
#'
#' @returns x
#'
#' @noRd
validate_codatamod <- function(x) {
  data <- x$data
  is_obs <- x$is_obs
  time <- x$time
  age <- x$age
  checkmate::assert_double(data,
    lower = 0
  )
  checkmate::assert_integer(is_obs,
    any.missing = FALSE,
    lower = 0L,
    upper = 1L
  )
  checkmate::assert_integer(time,
    any.missing = FALSE
  )
  checkmate::assert_integer(age,
    any.missing = FALSE,
    lower = 0L
  )
  ## is_obs is 0 iff data is NA
  is_consistent <- is.na(data) == (is_obs == 0L)
  i_inconsistent <- match(FALSE, is_consistent, nomatch = 0L)
  if (i_inconsistent > 0L) {
    stop(
      gettextf(
        "element %d of '%s' is %s but element %d of '%s' is %s",
        i_inconsistent,
        "data",
        data[[i_inconsistent]],
        i_inconsistent,
        "is_obs",
        is_obs[[i_inconsistent]]
      ),
      call. = FALSE
    )
  }
  ## return
  x
}


## HAS_TESTS
#' Checks specific to object of class "dpmaccount_codatamod_norm"
#'
#' @param x An object of class "dpmaccount_codatamod_norm"
#'
#' @returns x
#'
#' @noRd
validate_codatamod_norm <- function(x) {
  data <- x$data
  ratio <- x$ratio
  sd <- x$sd
  checkmate::assert_double(ratio, lower = 0)
  checkmate::assert_double(sd, lower = 0)
  ## 'ratio', 'sd' NA only if 'data' NA
  for (nm in c("ratio", "sd")) {
    val <- get(nm, x)
    is_na <- is.na(val) & !is.na(data)
    i_na <- match(TRUE, is_na, nomatch = 0L)
    if (i_na > 0L) {
      stop(
        gettextf(
          "element %d of '%s' is NA (and element %d of '%s' is not)",
          i_na,
          nm,
          i_na,
          "data"
        ),
        call. = FALSE
      )
    }
  }
  ## return
  x
}


## HAS_TESTS
#' Checks specific to object of class "dpmaccount_codatamod_binom"
#'
#' @param x An object of class "dpmaccount_codatamod_binom"
#'
#' @returns x
#'
#' @noRd
validate_codatamod_t <- function(x) {
  data <- x$data
  df <- x$df
  ratio <- x$ratio
  scale <- x$scale
  checkmate::assert_number(df, lower = 0)
  checkmate::assert_double(ratio, lower = 0)
  checkmate::assert_double(scale, lower = 0)
  ## 'ratio', 'scale' NA only if 'data' NA
  for (nm in c("ratio", "scale")) {
    val <- get(nm, x)
    is_na <- is.na(val) & !is.na(data)
    i_na <- match(TRUE, is_na, nomatch = 0L)
    if (i_na > 0L) {
      stop(
        gettextf(
          "element %d of '%s' is NA (and element %d of '%s' is not)",
          i_na,
          nm,
          i_na,
          "data"
        ),
        call. = FALSE
      )
    }
  }
  ## return
  x
}
