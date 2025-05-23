## HAS_TESTS
#' Specify a system model
#'
#' Specify a system model for rates for
#' - births,
#' - deaths,
#' - ins (eg inward migration)
#' - outs (eg outward migration).
#'
#' Rates for births, deaths, and outs are all expressed
#' as events per person-year lived, which is
#' the most common way of defining demographic rates.
#' However, **rates for ins are expressed as events
#' per year**, reflecting the fact that there is no
#' natural population at risk for inward movements.
#' When specifying rates for `ins`, it is important to
#' check that the rates are in fact defined as events
#' per year. For instance, if `ins` rates are classified by
#' age, period, and cohort, or by age, period, and Lexis
#' triangle, then they may have been defined as
#' events per half-year, rather than events per year.
#'
#' The model assumes that the rates are drawn from a
#' gamma distribution with shape parameter equal
#' to `1/disp`, and shape parameter equal to
#' `disp * mean`. When `disp` is zero, the rates
#' are treated as known.
#'
#' `mean` is a data frame with a variable called `mean`
#' containing non-negative numbers, and at least one
#' of the following variables:
#' - `age` Non-negative whole numbers, starting at 0
#' except in the case of births.
#' - `sex` Character or factor.
#' - `cohort` Birth cohort. Whole numbers, using the same
#' units as `time`.
#' - `time` Whole numbers. Normally years, but can be months
#' or quarters.
#'
#' The classifying variables that do occur in `mean` must
#' contain all categories that appear in the demographic account.
#'
#' `disp` can be a single non-negative number, or a data frame.
#' If `disp` is a data frame, then it is specified in the same
#' way as `mean`, except that the measurement variable is
#' called `disp` rather than `mean`.
#'
#' Rates for births should only cover the reproductive ages,
#' ie from approximately age 12 to approximately age 49
#' The exact cutoffs may depend on the births data.
#' It may sometimes be appropriate to ignore a few births at
#' very low or very high ages, to avoid very low rates,
#' which can create numerical problems with the estimates.
#'
#'
#' @section Multiple draws:
#' At present we only allow for single draws
#' for means and dispersions. We can extend this in
#' future by allowing for `value` columns that
#' are (for instance) lists or matrices.
#'
#' @param mean A data frame.
#' @param disp A data frame or a single number.
#' @param nm_series One of `"births"`, `"deaths"`,
#' `"ins"`, `"outs"`.
#'
#' @returns An object of class `"dpmaccount_sysmod"`.
#'
#' @examples
#' mean <- dpmaccount::gl_sysmod_mean_deaths
#' sysmod_deaths <- sysmod(
#'   mean = mean,
#'   disp = 0.2,
#'   nm_series = "deaths"
#' )
#' sysmod_deaths
#' @export
sysmod <- function(mean, disp = 0, nm_series) {
  min_mean <- 1e-6 ## these could potentially
  max_mean <- 10 ## be made visible to users
  nm_series <- check_and_tidy_nm_series(nm_series)
  check_is_events(nm_series)
  is_births <- nm_series == "births"
  is_ins <- nm_series == "ins"
  if (is_ins) {
    max_mean <- NULL
  }
  check_df_sysmod(
    df = mean,
    nm_df = "mean",
    is_births = is_births,
    nm_measure_var = "mean",
    min = min_mean,
    max = max_mean
  )
  mean <- tibble::tibble(mean)
  if (is.data.frame(disp)) {
    check_df_sysmod(
      df = disp,
      nm_df = "disp",
      is_births = is_births,
      nm_measure_var = "disp",
      min = NULL,
      max = NULL
    )
    disp <- tibble::tibble(disp)
  } else if (is.numeric(disp)) {
    checkmate::assert_number(disp,
      lower = 0
    )
  } else {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        "disp",
        class(disp)
      ),
      call. = FALSE
    )
  }
  new_sysmod(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
}


## HAS_TESTS
#' Create an object of class "dpmaccount_sysmod"
#'
#' @param mean A data frame
#' @param disp A data frame with the same
#' dimensions as 'mean'
#' @param nm_series Name of the demographic series
#' ("births", "deaths", "ins", or "outs").
#'
#' @return An object of class "dpmaccount_sysmod"
#'
#' @noRd
new_sysmod <- function(mean, disp, nm_series) {
  ans <- list(
    mean = mean,
    disp = disp,
    nm_series = nm_series
  )
  class(ans) <- "dpmaccount_sysmod"
  ans
}
