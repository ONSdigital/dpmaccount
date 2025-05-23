## Exact ----------------------------------------------------------------------

## HAS_TESTS
#' Create a data model for a series treated as error-free
#'
#' Create a data model in which the observed data are
#' treated as error-free and are copied straight into
#' the account and not subsequently updated.
#'
#' Data for deaths should cover the entire age range,
#' from 0 up to the maximum age.
#'
#' Data for births should only cover the reproductive ages,
#' ie from approximately age 12 to approximately age 49
#' The exact cutoffs may depend on the data.
#' It may sometimes be appropriate to ignore a few births at
#' very low or very high ages, to avoid very low rates,
#' which can create numerical problems with the estimates.
#'
#' `datamod_exact` is current the only valid data model for births
#' and deaths, and cannot be used for
#' any series other than births and deaths.
#'
#'
#' @section Data:
#'
#' `data` is a data frame holding reported counts. It must always
#' have the following variables:
#' - `age` Non-negative whole numbers. Must start at 0 in the case
#' of deaths; can start higher in the case of births.
#' - `sex` Character or factor. Must include the level `"Female"`.
#' Refers to the child in the case of births, and to the person
#' dying in the case of deaths.
#' - `time` Whole numbers. Typically years, but can be
#' quarters or months.
#' - `cohort` Birth year. Refers to the mother in the case of
#' births, and to the person dying in the case of deaths.
#' - `count` Non-negative numbers, not necessarily integer.
#'
#' `data` must include every combination of the classification
#' variables.
#'
#' @section nm_data:
#'
#' The name of the dataset. If no name is supplied,
#' then the name of the `nm_data` argument
#' is used instead.
#'
#' @section nm_series:
#'
#' The name of the demographic series that the
#' data refers to. One of
#' - `births`
#' - `deaths`
#'
#' @param data A data frame
#' @param nm_data Name of the dataset.
#' @param nm_series Name of the demographic series
#' that the data refers to.
#'
#' @returns An object of class `"dpmaccount_datamod_exact"`.
#'
#' @examples
#' deaths <- dpmaccount::gl_report_deaths
#' rep_deaths <- datamod_exact(deaths,
#'   nm_series = "deaths"
#' )
#' rep_deaths
#' @export
datamod_exact <- function(data,
                          nm_series,
                          nm_data = NULL) {
  if (is.null(nm_data)) {
    nm_data <- deparse1(substitute(data))
  } else {
    checkmate::assert_string(nm_data,
      min.chars = 1L
    )
  }
  nm_series <- check_and_tidy_nm_series(nm_series)
  check_is_bth_dth(nm_series)
  check_df_data(
    df = data,
    nm_df = "data",
    is_popn = FALSE
  )
  check_no_na(
    df = data,
    nm_x = "count",
    nm_df = "data"
  )
  data <- tibble::tibble(data)
  new_datamod_exact(
    data = data,
    nm_series = nm_series,
    nm_data = nm_data
  )
}

## HAS_TESTS
#' Create object of class "dpmaccount_datamod_exact"
#'
#' Assume inputs all checked and correct
#'
#' @param data A data frame
#' @param nm_series A string
#' @param nm_data A string
#'
#' @returns An object of class "dpmaccount_datamod_norm"
#'
#' @noRd
new_datamod_exact <- function(data, nm_series, nm_data) {
  ans <- list(
    data = data,
    nm_series = nm_series,
    nm_data = nm_data
  )
  class(ans) <- c("dpmaccount_datamod_exact", "dpmaccount_datamod")
  ans
}


## Normal ---------------------------------------------------------------------

## HAS_TESTS
#' Specify a data model based on a normal distribution
#'
#' Create a data model where the relationship between the
#' reported and true counts is represented by a
#' normal distribution.
#'
#' When `scale_ratio` and `scale_sd` are both `0`
#' (the default), the data model is
#'
#' \deqn{y_i \sim \mathcal{N}(\rho_i x_i, \sigma_i^2)}
#'
#' where
#'
#' - \eqn{y_i} is the observed count for cell \eqn{i}
#' - \eqn{\rho_i} is the net coverage ratio for cell \eqn{i}
#' - \eqn{x_i} is the true count for cell \eqn{i}, and
#' - \eqn{\sigma_i} is the standard deviation for cell \eqn{i}.
#'
#' When `scale_ratio` and `scale_sd` are
#' greater than `0`,
#' the data model becomes
#'
#' \deqn{y_i \sim \mathcal{N}(e^{\alpha_{c_i}} \rho_i x_i, (e^{\beta_{c_i}} \sigma_i)^2)}
#' \deqn{\alpha_c \sim \mathcal{N}(0, A_{\alpha}^2)}
#' \deqn{\beta_c \sim \mathcal{N}(0, A_{\beta}^2)}
#'
#' where
#'
#' - \eqn{c_i} is the cohort associated with cell \eqn{i},
#' - \eqn{\alpha_c} is multiplier applied to the coverage
#'   ratio for cohort \eqn{c},
#' - \eqn{\beta_c} is multiplier applied to the standard deviation
#'   for cohort \eqn{c},
#' - \eqn{A_{\alpha}} is `scale_ratio`, and
#' - \eqn{A_{\beta}} is `scale_sd`.
#'
#' In the extended model, \eqn{\alpha_c}
#' and \eqn{\beta_c} are
#' treated as unknown, and are estimated
#' when [estimate_account()]
#' is called.
#'
#' @section data:
#'
#' `data` is a data frame holding reported counts. It must always
#' have the following variables:
#' - `age` Non-negative whole numbers, starting at 0.
#' - `sex` Character or factor. Must include the level `"Female"`.
#' - `time` Whole numbers. Typically years, but can be
#' quarters or months.
#' - `count` Non-negative numbers, not necessarily integer.
#'
#' If the data being modelled describe ins or outs,
#' then `data` must also include a variable
#' called `cohort`, which uses the same units as `time`.
#' If the data being modelled describe population,
#' then `data` must not have a variable called cohort.
#'
#' If a combination of classifying variables is missing from
#' `data`, then `datamod_norm` assumes that the corresponding
#' value for `count` is `NA` (not `0`).
#'
#' @section ratio:
#'
#' The `ratio` argument governs net coverage ratios:
#' the number of reported events or people expected for each
#' actual event or person. If a data source is unbiased,
#' then its coverage ratio is 1.
#'
#' `ratio` can be a single number or a data frame.
#' If `ratio` is a single number, then the
#' same coverage ratio is used for all values of
#' the data. If `ratio` is a data frame,
#' then different coverage rates are used for
#' different parts of the data.
#' If a classifying variable in `data` is
#' omitted from `ratio`,  then the coverage ratio
#' is assumed to be constant
#' across all levels of the omitted variable.
#' Data frame `ratio` must have a column called `ratio`.
#'
#' @section sd:
#'
#' The `sd` argument governs the standard deviation of
#' variability around the expected value. In general,
#' the less reliable the data source is,
#' the higher `sd` should be.
#'
#' Unlike `ratio`, `sd` can only be a data frame.
#' It rarely makes sense to use the same value for
#' `sd` across multiple values for the data.
#'
#' Data frame `sd` must have a variable called `sd`.
#' All values for the `sd` variable must be positive
#' (ie greater than zero).
#'
#' @section scale_ratio, scale_sd:
#'
#' Specifying non-zero values for `scale_ratio` or
#' `scale_sd` adds flexibility to the data model.
#' A non-zero value for `scale_ratio` implies that we
#' may have systematically under-estimated or overestimated
#' coverage ratios.
#' A non-zero value for `scale_sd` implies that
#' we may have systematically under-estimated or
#' overestimated uncertainty. Setting
#' `ratio_sd` to `0.1`, for instance, implies that
#' the coverage ratios implied by `ratio` could consistently
#' understate or overstate coverage ratios, by 10 percent
#' or even 20-30 percent.
#'
#' @section nm_data:
#'
#' The name of the dataset. If no name is supplied,
#' then the name of the `data` argument
#' is used instead.
#'
#' @section nm_series:
#'
#' The name of the demographic series that the
#' data refers to. One of
#' - `"population"`
#' - `"ins"`
#' - `"outs"`
#'
#' @param data A data frame
#' @param ratio A single number, or a data frame.
#' Defaults to 1.
#' @param sd A data frame.
#' @param scale_ratio A non-negative number.
#' Default is 0.
#' @param scale_sd A non-negative number.
#' Default is 0.
#' @param nm_data Name of the dataset.
#' @param nm_series Name of the demographic series
#' that the data refers to.
#'
#' @return An object of class `"dpmaccount_datamod_norm"`.
#'
#' @examples
#' reg_popn <- dpmaccount::gl_report_popn
#' cover_ratio <- dpmaccount::gl_cover_ratio_popn
#' cover_sd <- dpmaccount::gl_cover_sd_popn
#'
#' ## specify ratio and sd
#' datamod_norm(
#'   data = reg_popn,
#'   ratio = cover_ratio,
#'   sd = cover_sd,
#'   nm_series = "population"
#' )
#'
#' ## assume data source unbiased - use
#' ## default, ratio = 1
#' datamod_norm(
#'   data = reg_popn,
#'   sd = cover_sd,
#'   nm_series = "population"
#' )
#'
#' ## non-zero scale_ratio
#' datamod_norm(
#'   data = reg_popn,
#'   sd = cover_sd,
#'   scale_ratio = 0.1,
#'   nm_series = "population"
#' )
#' @export
datamod_norm <- function(data,
                         ratio = 1,
                         sd,
                         scale_ratio = 0,
                         scale_sd = 0,
                         nm_series,
                         nm_data = NULL) {
  if (is.null(nm_data)) {
    nm_data <- deparse1(substitute(data))
  } else {
    checkmate::assert_string(nm_data,
      min.chars = 1L
    )
  }
  check_nm_data_clash(nm_data)
  nm_series <- check_and_tidy_nm_series(nm_series)
  check_is_not_bth_dth(nm_series)
  is_popn <- identical(nm_series, "population")
  check_df_data(
    df = data,
    nm_df = "data",
    is_popn = is_popn
  )
  check_scale(
    x = scale_ratio,
    x_arg = "scale_ratio",
    zero_ok = TRUE
  )
  check_scale(
    x = scale_sd,
    x_arg = "scale_sd",
    zero_ok = TRUE
  )
  if (is.numeric(ratio)) {
    checkmate::assert_number(ratio,
      lower = 0,
      finite = TRUE
    )
  } else if (is.data.frame(ratio)) {
    check_df_dataconst(
      df = ratio,
      nm_df = "ratio",
      nm_measure_var = "ratio",
      data = data,
      is_popn = is_popn
    )
  } else {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        "ratio",
        class(ratio)
      ),
      call. = FALSE
    )
  }
  check_df_dataconst(
    df = sd,
    nm_df = "sd",
    nm_measure_var = "sd",
    data = data,
    is_popn = is_popn
  )
  is_pos <- sd$sd > 0
  i_nonpos <- match(FALSE, is_pos, nomatch = 0L)
  if (i_nonpos > 0L) {
    stop(
      gettextf(
        "non-positive value [%s] for variable '%s' : row %d of data frame '%s' in data model for dataset \"%s\"",
        sd$sd[[i_nonpos]], "sd", i_nonpos, "sd", nm_data
      ),
      call. = FALSE
    )
  }
  if (is_popn) {
    data$cohort <- with(data, time - age)
  }
  data <- tibble::tibble(data)
  new_datamod_norm(
    data = data,
    ratio = ratio,
    sd = sd,
    scale_ratio = scale_ratio,
    scale_sd = scale_sd,
    nm_series = nm_series,
    nm_data = nm_data
  )
}


## HAS_TESTS
#' Create object of class "dpmaccount_datamod_norm"
#'
#' Assume inputs all checked and correct
#'
#' @param data A data frame
#' @param ratio A single non-negative number
#' or a data frame
#' @param sd A data frame
#' @param scale_ratio Non-negative number
#' @param scale_sd Non-negative number
#' @param nm_series A string
#' @param nm_data A string
#'
#' @returns An object of class "dpmaccount_datamod_norm"
#'
#' @noRd
new_datamod_norm <- function(data,
                             ratio,
                             sd,
                             scale_ratio,
                             scale_sd,
                             nm_series,
                             nm_data) {
  ans <- list(
    data = data,
    ratio = ratio,
    sd = sd,
    scale_ratio = scale_ratio,
    scale_sd = scale_sd,
    nm_series = nm_series,
    nm_data = nm_data
  )
  class(ans) <- c(
    "dpmaccount_datamod_norm",
    "dpmaccount_datamod"
  )
  ans
}


## t --------------------------------------------------------------------------

## HAS_TESTS
#' Specify a data model based on a t distribution
#'
#' Create a data model where the relationship between the
#' reported and true counts is represented by a
#' [t distribution](https://en.wikipedia.org/wiki/Student%27s_t-distribution#Location-scale_t-distribution)
#' with location and scale parameters
#'
#' When `scale_ratio` is zero (the default) the data model is
#'
#' \deqn{y_i \sim t(\nu, \rho_i x_i, s_i^2)}
#'
#' where
#'
#' - \eqn{y_i} is the observed count for cell \eqn{i},
#' - \eqn{t} is a location-scale t distribution,
#' - \eqn{\nu} is the degrees of freedom,
#' - \eqn{\rho_i} is the coverage ratio for cell \eqn{i},
#' - \eqn{x_i} is the true count for cell \eqn{i}, and
#' - \eqn{s_i} is the scale for cell \eqn{i}.
#'
#' When `scale_ratio` is greater than zero the data model becomes
#'
#'  \deqn{y_i \sim t(\nu, e^{\alpha_{c_i}} \rho_i x_i, s_i^2)}
#'  \deqn{\alpha_c \sim \mathcal{N}(0, A_{\alpha}^2)}
#'
#' - \eqn{c_i} is the cohort associated with cell \eqn{i},
#' - \eqn{\alpha_c} is multiplier applied to the coverage
#'   ratio for cohort \eqn{c}
#'
#' In the extended model, \eqn{\alpha_c}
#' is treated as unknown, and is estimated
#' when [estimate_account()] is called.
#'
#' @section data:
#'
#' `data` is a data frame holding reported counts. It must always
#' have the following variables:
#' - `age` Non-negative whole numbers, starting at 0.
#' - `sex` Character or factor. Must include the level `"Female"`.
#' - `time` Whole numbers. Typically years, but can be
#' quarters or months.
#' - `count` Non-negative numbers, not necessarily integer.
#'
#' If the data being modelled describe ins or outs,
#' then `data` must also include a variable
#' called `cohort`, which uses the same units as `time`.
#' If the data being modelled describe population,
#' then `data` must not have a variable called cohort.
#'
#' If a combination of classifying variables is missing from
#' `data`, then `datamod_t` assumes that the corresponding
#' value for `count` is `NA` (not `0`).
#'
#' @section df:
#'
#' Degrees of freedom. A positive number. Non-integer
#' values are allowed, but `Inf` is not. (Setting
#' `df` to `Inf` is equivalent to using
#' [datamod_norm()].)
#'
#' A \eqn{t} distribution look like a normal distribution,
#' except that it has thicker tails. This means that
#' extremely low values, or extremely high values,
#' are more common with a \eqn{t} distribution. The
#' smaller the value of `df`, the thicker the tails,
#' and the more common extreme values become.
#'
#' @inheritSection datamod_norm ratio
#'
#' @section scale:
#'
#' The `scale` argument governs dispersion
#' around the expected value. In general,
#' the less reliable the data source is,
#' the higher `scale` should be.
#'
#' Unlike `ratio`, `scale` must be a data frame.
#' It rarely makes sense to use the same value for
#' `scale` across multiple values for the data.
#'
#' Data frame `scale` must have a variable called `scale`.
#' All values for the `scale` variable must be positive
#' (ie greater than zero).
#'
#' @section scale_ratio:
#'
#' Specifying a non-zero value for `scale_ratio` adds
#' flexibility to the data model.
#' A non-zero value for `scale_ratio` implies that we
#' may have systematically under-estimated or overestimated
#' coverage ratios.
#' Setting `scale_ratio` to `0.1`, for instance, implies that
#' the coverage ratios implied by `ratio` could consistently
#' understate or overstate coverage ratios, by 10 percent
#' or even 20-30 percent.
#'
#' @inheritSection datamod_norm nm_data
#'
#' @inheritSection datamod_norm nm_series
#'
#' @inheritParams datamod_norm
#' @param df Degrees of freedom. A positive
#' real number. Defaults to 4.
#' @param scale Values governing dispersion
#' (equivalent to `sd` in normal models.)
#' A data frame with a column called `scale`.
#'
#' @return An object of class `"dpmaccount_datamod_t"`.
#'
#' @examples
#' ## No bias in coverage ratio - scale_ratio = 0
#' reg_popn <- dpmaccount::gl_report_popn
#' scale <- dpmaccount::gl_cover_sd_popn
#' names(scale)[match("sd", names(scale))] <- "scale"
#' datamod_t(
#'   data = reg_popn,
#'   scale = scale,
#'   scale_ratio = 0,
#'   nm_series = "population"
#' )
#'
#' ## non-zero scale ratio
#' #' reg_popn <- dpmaccount::gl_report_popn
#' scale <- dpmaccount::gl_cover_sd_popn
#' names(scale)[match("sd", names(scale))] <- "scale"
#' datamod_t(
#'   data = reg_popn,
#'   scale = scale,
#'   scale_ratio = 0.1,
#'   nm_series = "population"
#' )
#'
#' @export
datamod_t <- function(data,
                      df = 4,
                      ratio = 1,
                      scale,
                      scale_ratio = 0,
                      nm_series,
                      nm_data = NULL) {
  if (is.null(nm_data)) {
    nm_data <- deparse1(substitute(data))
  } else {
    checkmate::assert_string(nm_data,
      min.chars = 1L
    )
  }
  check_nm_data_clash(nm_data)
  nm_series <- check_and_tidy_nm_series(nm_series)
  check_is_not_bth_dth(nm_series)
  is_popn <- identical(nm_series, "population")
  check_df_data(
    df = data,
    nm_df = "data",
    is_popn = is_popn
  )
  check_scale(
    x = scale_ratio,
    x_arg = "scale_ratio",
    zero_ok = TRUE
  )
  check_df(df)
  df <- as.double(df)
  if (is.numeric(ratio)) {
    checkmate::assert_number(ratio,
      lower = 0,
      finite = TRUE
    )
  } else if (is.data.frame(ratio)) {
    check_df_dataconst(
      df = ratio,
      nm_df = "ratio",
      nm_measure_var = "ratio",
      data = data,
      is_popn = is_popn
    )
  } else {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        "ratio",
        class(ratio)
      ),
      call. = FALSE
    )
  }
  check_df_dataconst(
    df = scale,
    nm_df = "scale",
    nm_measure_var = "scale",
    data = data,
    is_popn = is_popn
  )
  is_pos <- scale$scale > 0
  i_nonpos <- match(FALSE, is_pos, nomatch = 0L)
  if (i_nonpos > 0L) {
    stop(
      gettextf(
        "non-positive value [%s] for variable '%s' : row %d of data frame '%s' in data model for dataset \"%s\"",
        scale$scale[[i_nonpos]], "scale", i_nonpos, "scale", nm_data
      ),
      call. = FALSE
    )
  }
  if (is_popn) {
    data$cohort <- with(data, time - age)
  }
  data <- tibble::tibble(data)
  new_datamod_t(
    data = data,
    df = df,
    ratio = ratio,
    scale = scale,
    scale_ratio = scale_ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
}


## HAS_TESTS
#' Create object of class "dpmaccount_datamod_t"
#'
#' Assume inputs all checked and correct
#'
#' @param data A data frame
#' @param df A positive number
#' @param ratio A single non-negative number
#' or a data frame
#' @param scale A data frame
#' @param scale_ratio A non-negative number
#' @param nm_series A string
#' @param nm_data A string
#'
#' @returns An object of class "dpmaccount_datamod_t"
#'
#' @noRd
new_datamod_t <- function(data,
                          df,
                          ratio,
                          scale,
                          scale_ratio,
                          nm_series,
                          nm_data) {
  ans <- list(
    data = data,
    df = df,
    ratio = ratio,
    scale = scale,
    scale_ratio = scale_ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
  class(ans) <- c("dpmaccount_datamod_t", "dpmaccount_datamod")
  ans
}


## Negative binomial ----------------------------------------------------------

## HAS_TESTS
#' Specify a data model based on a negative binomial distribution
#'
#' Create a data model where the relationship between the
#' reported and true population counts is represented by a negative
#' binomial distribution. In this model, the negative binomial
#' is given a mean-dispersion parameterisation. (For a discussion
#' of the mean-disperion parameterisation, see, for instance,
#' \href{https://en.wikipedia.org/wiki/Negative_binomial_distribution}{wikipedia}
#' or \href{https://mc-stan.org/docs/2_20/functions-reference/nbalt.html}{stan docs}).
#'
#' Let \eqn{y} be the reported count and \eqn{x} the true count.
#' Let \eqn{r} and \eqn{d} be ratio and dispersion parameters.
#' Let \eqn{v = 1/d}. When \eqn{d > 0} and `scale_ratio` takes the default value of zero, the probability mass function
#' is
#' \deqn{
#' Pr(Y=y) = \Gamma(v + y) / (y! \Gamma(v))
#'     (v / (v + r x))^v
#'     ((r x) / (v + r x))^y
#' }
#' When \eqn{d=0}, the distribution theoretically reduces to the Poisson distribution.
#' However, within dpmaccount, \eqn{d=0} is currently not allowed for the negative binomial
#' datamodel and an error is returned.
#' For dispersion only the following values are acceptable \eqn{d > 1e-6}.
#'
#'
#' When `scale_ratio` is greater than zero the data model becomes
#' \deqn{
#' Pr(Y=y) = \Gamma(v + ye^{\alpha_{c_i}}) / ((ye^{\alpha_{c_i}})! \Gamma(v))
#'     (v / (v + r x))^v
#'     ((r x) / (v + r x))^(ye^{\alpha_{c_i}})
#' }
#'  \deqn{\alpha_c \sim \mathcal{N}(0, A_{\alpha}^2)}
#'
#' - \eqn{c_i} is the cohort associated with cell \eqn{i},
#' - \eqn{\alpha_c} is multiplier applied to the coverage
#'   ratio for cohort \eqn{c}
#'
#' In the extended model, \eqn{\alpha_c}
#' is treated as unknown, and is estimated
#' when [estimate_account()] is called.
#'
#' @section data:
#'
#' `data` is a data frame holding reported counts. It must always
#' have the following variables:
#' - `age` Non-negative whole numbers, starting at 0.
#' - `sex` Character or factor. Must include the level `"Female"`.
#' - `time` Whole numbers. Typically years, but can be
#' quarters or months.
#' - `count` Non-negative numbers, not necessarily integer.
#'
#' If the data being modelled describe ins or outs,
#' then `data` must also include a variable
#' called `cohort`, which uses the same units as `time`.
#' If the data being modelled describe population,
#' then `data` must not have a variable called cohort.
#'
#' If a combination of classifying variables is missing from
#' `data`, then `datamod_nbinom` assumes that the corresponding
#' value for `count` is `NA` (not `0`).
#'
#' @inheritSection datamod_norm ratio
#'
#' @inheritSection datamod_norm nm_data
#'
#' @inheritSection datamod_norm nm_series
#'
#' @section disp:
#' The \code{disp} argument governs dispersion, ie variability.
#' When \code{disp} equals 0, variance in the reported
#' count equals the expected value for the reported count.
#' When \code{disp} is greater than 0, variance in the reported
#' count is greater than the expected value. In general,
#' the less reliable the data source is,
#' the higher \code{disp} should be.
#'
#' The \code{disp} argument can be a single number,
#' or can be a data frame with some or all of
#' same variables as \code{data},
#' except for \code{"count"},
#' which is replaced by a \code{"disp"}.
#'
#' @section scale_ratio:
#'
#' Specifying a non-zero value for `scale_ratio` adds
#' flexibility to the data model.
#' A non-zero value for `scale_ratio` implies that we
#' may have systematically under-estimated or overestimated
#' coverage ratios.
#' Setting `scale_ratio` to `0.1`, for instance, implies that
#' the coverage ratios implied by `ratio` could consistently
#' understate or overstate coverage ratios, by 10 percent
#' or even 20-30 percent.
#'
#' @param data A data frame
#' @param ratio A single number, or a data frame. (Default is 1.).
#' @param disp A single number, or a data frame with a variable called \code{"disp"}. (Default is 1. Must be greater than 1e-6)
#' @param scale_ratio A non-negative number. (Default is 0).
#' @param nm_data Name of the dataset.
#' @param nm_series Name of the demographic series that the data refers to.
#'
#' @return An object of class \code{"datamod_nbinom"}.
#'
#' @examples
#' ## A constant coverage ratio (of 90%)
#' ## across all categories,
#' ## but higher dispersion for young males,
#' ## than for other groups
#' reg_popn <- dpmaccount::gl_report_popn
#' disp <- within(reg_popn, {
#'   rm(count)
#'   disp <- ifelse(sex == "Male" & age %in% 20:29, 0.01, 0.05)
#' })
#' varying_disp_dm <- datamod_nbinom(
#'   data = reg_popn,
#'   ratio = 0.9,
#'   disp = disp,
#'   nm_series = "population"
#' )
#' varying_disp_dm
#'
#' #' ## A constant but likely biased coverage ratio (of 90%)
#' ## across all categories,
#' ## but higher dispersion for young males,
#' ## than for other groups
#' reg_popn <- dpmaccount::gl_report_popn
#' disp <- within(reg_popn, {
#'   rm(count)
#'   disp <- ifelse(sex == "Male" & age %in% 20:29, 0.01, 0.05)
#' })
#' varying_disp_dm <- datamod_nbinom(
#'   data = reg_popn,
#'   ratio = 0.9,
#'   disp = disp,
#'   scale_ratio = 0.1,
#'   nm_series = "population"
#' )
#' varying_disp_dm
#'
#' @export
datamod_nbinom <- function(data,
                           ratio = 1,
                           disp = 1,
                           scale_ratio = 0,
                           nm_series,
                           nm_data = NULL) {
  if (is.null(nm_data)) {
    nm_data <- deparse1(substitute(data))
  } else {
    checkmate::assert_string(nm_data,
      min.chars = 1L
    )
  }
  check_nm_data_clash(nm_data)
  nm_series <- check_and_tidy_nm_series(nm_series)
  check_is_not_bth_dth(nm_series)
  is_popn <- identical(nm_series, "population")
  check_scale(
    x = scale_ratio,
    x_arg = "scale_ratio",
    zero_ok = TRUE
  )
  check_df_data(
    df = data,
    nm_df = "data",
    is_popn = is_popn
  )
  if (is.numeric(ratio)) {
    checkmate::assert_number(ratio,
      lower = 0,
      finite = TRUE
    )
  } else if (is.data.frame(ratio)) {
    check_df_dataconst(
      df = ratio,
      nm_df = "ratio",
      nm_measure_var = "ratio",
      data = data,
      is_popn = is_popn
    )
  } else {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        "ratio",
        class(ratio)
      ),
      call. = FALSE
    )
  }
  if (is.numeric(disp)) {
    checkmate::assert_number(disp,
      lower = 1e-6,
      finite = TRUE
    )
  } else if (is.data.frame(disp)) {
    check_df_dataconst(
      df = disp,
      nm_df = "disp",
      nm_measure_var = "disp",
      data = data,
      is_popn = is_popn
    )
    is_pos <- disp$disp > 0
    i_nonpos <- match(FALSE, is_pos, nomatch = 0L)
    if (i_nonpos > 0L) {
      stop(
        gettextf(
          "non-positive value [%s] for variable '%s' : row %d of data frame '%s' in data model for dataset \'%s\'",
          disp$disp[[i_nonpos]], "disp", i_nonpos, "disp", nm_data
        ),
        call. = FALSE
      )
    }
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

  classif_vars <- data[-match("count", names(data))]
  if (is.numeric(ratio)) {
    ratio <- rep(ratio, times = nrow(classif_vars))
    ratio <- data.frame(classif_vars, ratio)
  } else {
    ratio <- merge(classif_vars, ratio, all.x = TRUE)
  }
  if (is.numeric(disp)) {
    disp <- rep(disp, times = nrow(classif_vars))
    disp <- data.frame(classif_vars, disp)
  } else {
    disp <- merge(classif_vars, disp, all.x = TRUE)
  }
  if (is_popn) {
    data$cohort <- with(data, time - age)
  }
  data <- tibble::tibble(data)
  new_datamod_nbinom(
    data = data,
    ratio = ratio,
    disp = disp,
    scale_ratio = scale_ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
}

## HAS_TESTS
#' Create object of class "dpmaccount_datamod_nbinom"
#'
#' Assume inputs all checked and correct
#'
#' @param data A data frame
#' @param ratio A single non-negative number, or a data frame
#' @param disp A single non-negative number number, or a data frame with a variable called \code{"disp"}
#' @param scale_ratio A non-negative number
#' @param nm_series A string
#' @param nm_data A string
#'
#' @returns An object of class "dpmaccount_datamod_nbinom"
#'
#' @noRd
new_datamod_nbinom <- function(data,
                               ratio = 1,
                               disp = 1,
                               scale_ratio = 0,
                               nm_series,
                               nm_data = NULL) {
  ans <- list(
    data = data,
    ratio = ratio,
    disp = disp,
    scale_ratio = scale_ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
  class(ans) <- c("dpmaccount_datamod_nbinom", "dpmaccount_datamod")
  ans
}

## Poisson ----------------------------------------------------------

## HAS_TESTS
#' Specify a data model based on a Poisson distribution
#'
#' Create a data model where the relationship between the
#' reported and true population counts is represented by a Poisson distribution.
#'
#' #' When `scale_ratio` takes the default value of zero, the Poisson distribution
#' has probability mass function
#' \deqn{
#' Pr(X=k) = (1/(k!)) exp(-\lambda) (\lambda)^k
#' } and density
#' \deqn{
#' p(x|\lambda) = \lambda^x exp(-\lambda)/x!
#' } with mean and variance E(X) = Var(X) = \eqn{\lambda}.
#'
#'
#' When `scale_ratio` is greater than zero the data model becomes
#' \deqn{
#' p(x|\lambda) = \lambda^{\alpha_{c_{i}} x} exp(-\lambda)/(\alpha_{c_{i}} x)!
#' }
#'  \deqn{\alpha_{c_{i}} \sim \mathcal{N}(0, A_{\alpha_{c_{i}}}^2)}
#'
#' - \eqn{c_i} is the cohort associated with cell \eqn{i},
#' - \eqn{\alpha_c} is multiplier applied to the coverage
#'   ratio for cohort \eqn{c}
#'
#' In the extended model, \eqn{\alpha_{c_{i}}}
#' is treated as unknown, and is estimated
#' when [estimate_account()] is called.
#'
#' @section data:
#'
#' `data` is a data frame holding reported counts. It must always
#' have the following variables:
#' - `age` Non-negative whole numbers, starting at 0.
#' - `sex` Character or factor. Must include the level `"Female"`.
#' - `time` Whole numbers. Typically years, but can be
#' quarters or months.
#' - `count` Non-negative numbers, not necessarily integer.
#'
#' If the data being modelled describe ins or outs,
#' then `data` must also include a variable
#' called `cohort`, which uses the same units as `time`.
#' If the data being modelled describe population,
#' then `data` must not have a variable called cohort.
#'
#' If a combination of classifying variables is missing from
#' `data`, then `datamod_poisson` assumes that the corresponding
#' value for `count` is `NA` (not `0`).
#'
#' @inheritSection datamod_norm ratio
#'
#' @section scale_ratio:
#'
#' Specifying a non-zero value for `scale_ratio` adds
#' flexibility to the data model.
#' A non-zero value for `scale_ratio` implies that we
#' may have systematically under-estimated or overestimated
#' coverage ratios.
#' Setting `scale_ratio` to `0.1`, for instance, implies that
#' the coverage ratios implied by `ratio` could consistently
#' understate or overstate coverage ratios, by 10 percent
#' or even 20-30 percent.
#'
#' @inheritSection datamod_norm nm_data
#'
#' @inheritSection datamod_norm nm_series
#'
#'
#' @param data A data frame
#' @param ratio A single number, or a data frame. (Default is 1.).
#' @param scale_ratio A non-negative number. (Default is 0).
#' @param nm_data Name of the dataset.
#' @param nm_series Name of the demographic series that the data refers to.
#'
#' @return An object of class \code{"datamod_poisson"}.
#'
#' @examples
#' ## A constant coverage ratio (of 90%)
#' ## across all categories,
#' ## but higher mean rate for young males,
#' ## than for other group
#' reg_popn <- dpmaccount::gl_report_popn
#' poisson_dm <- datamod_poisson(
#'   data = reg_popn,
#'   ratio = 0.9,
#'   nm_series = "population"
#' )
#' poisson_dm
#'
#' ## A constant but likely biased coverage ratio (of 90%)
#' ## across all categories,
#' ## but higher mean rate for young males,
#' ## than for other group
#' reg_popn <- dpmaccount::gl_report_popn
#' poisson_dm <- datamod_poisson(
#'   data = reg_popn,
#'   ratio = 0.9,
#'   scale_ratio = 0.1,
#'   nm_series = "population"
#' )
#' poisson_dm
#'
#' @export
datamod_poisson <- function(data,
                            ratio = 1,
                            scale_ratio = 0,
                            nm_series,
                            nm_data = NULL) {
  if (is.null(nm_data)) {
    nm_data <- deparse1(substitute(data))
  } else {
    checkmate::assert_string(nm_data,
      min.chars = 1L
    )
  }
  check_nm_data_clash(nm_data)
  nm_series <- check_and_tidy_nm_series(nm_series)
  check_is_not_bth_dth(nm_series)
  is_popn <- identical(nm_series, "population")
  check_scale(
    x = scale_ratio,
    x_arg = "scale_ratio",
    zero_ok = TRUE
  )
  check_df_data(
    df = data,
    nm_df = "data",
    is_popn = is_popn
  )

  if (is.numeric(ratio)) {
    checkmate::assert_number(ratio,
      lower = 0,
      finite = TRUE
    )
  } else if (is.data.frame(ratio)) {
    check_df_dataconst(
      df = ratio,
      nm_df = "ratio",
      nm_measure_var = "ratio",
      data = data,
      is_popn = is_popn
    )
  } else {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        "ratio",
        class(ratio)
      ),
      call. = FALSE
    )
  }

  classif_vars <- data[-match("count", names(data))]
  if (is.numeric(ratio)) {
    ratio <- rep(ratio, times = nrow(classif_vars))
    ratio <- data.frame(classif_vars, ratio)
  } else {
    ratio <- merge(classif_vars, ratio, all.x = TRUE)
  }
  if (is_popn) {
    data$cohort <- with(data, time - age)
  }
  data <- tibble::tibble(data)
  new_datamod_poisson(
    data = data,
    ratio = ratio,
    scale_ratio = scale_ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
}

## HAS_TESTS
#' Create object of class "dpmaccount_datamod_poisson"
#'
#' Assume inputs all checked and correct
#'
#' @param data A data frame
#' @param ratio A single non-negative number, or a data frame
#' @param scale_ratio A non-negative number
#' @param nm_series A string
#' @param nm_data A string
#'
#' @returns An object of class "dpmaccount_datamod_poisson"
#'
#' @noRd
new_datamod_poisson <- function(data,
                                ratio = 1,
                                scale_ratio = 0,
                                nm_series,
                                nm_data = NULL) {
  ans <- list(
    data = data,
    ratio = ratio,
    scale_ratio = scale_ratio,
    nm_series = nm_series,
    nm_data = nm_data
  )
  class(ans) <- c("dpmaccount_datamod_poisson", "dpmaccount_datamod")
  ans
}
