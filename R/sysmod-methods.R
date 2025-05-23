## 'get_classif_vars' ---------------------------------------------------------

## generic defined in R/datamod-methods.R

## HAS_TESTS
#' @export
get_classif_vars.dpmaccount_sysmod <- function(x) {
  mean <- x$mean
  i_mean <- match("mean", names(mean))
  mean[-i_mean]
}


## 'get_nm_series' ------------------------------------------------------------

## generic defined in R/datamod-methods.R

## HAS_TESTS
#' @export
get_nm_series.dpmaccount_sysmod <- function(x) x$nm_series


## 'make_cosysmod_df' ---------------------------------------------------------

#' Convert a user-supplied system model into cohort-level
#' values for mean and dispersion
#'
#' Turn an 'dpmaccount_sysmod' object into a data frame
#' with the following columns:
#' - 'cohort'
#' - 'sex'
#' - 'sysmod_*', where * is "bth", "dth", "ins", or "outs"
#'
#' The 'sysmod' column is a list column, each element of which
#' is a data frame with the following columns:
#' - 'mean'
#' - 'disp'
#' - 'time'
#' - 'age'
#'
#' 'mean' and 'disp' in 'mod' do not necessarily contain
#' all classiciation variables, but if they do contain
#' a classification variable, then that variable has all
#' possible categories.
#'
#' Note that the 'time' and 'age' columns in the
#' result aren't used in the estimation - we keep
#' them as a form of documentation, eg for use
#' in debugging.
#'
#' @param mod An object of class 'dpmaccount_sysmod'
#' @param classif_vars A data frame with complete
#' classification variables.
#'
#' @returns A data frame
#'
#' @noRd
make_cosysmod_df <- function(mod, classif_vars) {
  UseMethod("make_cosysmod_df")
}


## HAS_TESTS
#' @export
make_cosysmod_df.dpmaccount_sysmod <- function(mod, classif_vars) {
  mean <- mod$mean
  disp <- mod$disp
  nm_series <- mod$nm_series
  df <- merge(classif_vars, mean, all.x = TRUE)
  if (is.data.frame(disp)) {
    df <- merge(df, disp)
  } else {
    df$disp <- disp
  }
  ord <- with(df, order(cohort, sex, age, time))
  df <- df[ord, ]
  nm_value <- switch(nm_series,
    births = "sysmod_bth",
    deaths = "sysmod_dth",
    ins = "sysmod_ins",
    outs = "sysmod_outs"
  )
  ans <- nest_to_df(
    df = df,
    nms_data = c("mean", "disp", "time", "age"),
    nms_group = c("cohort", "sex"),
    nm_value = nm_value
  )
  ans
}



## print ----------------------------------------------------------------------

#' @export
print.dpmaccount_sysmod <- function(x, ...) {
  nchar_offset <- 10
  cat("System model : An object of class \"dpmaccount_sysmod\"\n\n")
  cat("series:", x$nm_series, "\n\n")
  cat("mean:\n")
  print(summary(x$mean))
  if (is.data.frame(x$disp)) {
    cat("\ndisp:\n")
    print(summary(x$disp))
  } else {
    cat("\ndisp: ", x$disp, "\n", sep = "")
  }
  invisible(x)
}
