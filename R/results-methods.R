## 'as_tibble' ----------------------------------------------------------------

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' Coerce a results object to a tibble
#'
#' Convert an object of class `"dpmaccount_results"` into
#' a [tibble][tibble::tibble()].
#'
#' @param x An object of class `"dpmaccount_results"`.
#' @param ... Additional arguments passed to
#' [tibble::tibble()].
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' An object of class `"dpmaccount_results"`
#' is typically created by a call to [estimate_account()].
#'
#' @examples
#' df <- as_tibble(results_greenland)
#' @rdname as_tibble
#' @export
as_tibble.dpmaccount_results <- function(x, ...) {
  has_adfun <- "adfun" %in% names(x)
  if (has_adfun) {
    tibble::tibble(
      cohort = x$cohort,
      sex = x$sex,
      fitted = x$fitted,
      adfun = x$adfun,
      ...
    )
  } else {
    tibble::tibble(
      cohort = x$cohort,
      sex = x$sex,
      fitted = x$fitted,
      ...
    )
  }
}


## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## HAS_TESTS
#' Extract estimates or input data
#'
#' Extract estimated counts and rates, or input data,
#' from a results object created by
#' [estimate_account()].
#'
#' Argument `what` can take one or more of the
#' following values:
#' - `"population"` Estimated population counts.
#' - `"events"` Estimated counts of births, deaths, ins, outs.
#' - `"exposure"` Estimated person-years of exposure.
#' - `"rates"` Estimated rates for births, deaths, ins, outs.
#' - `"sysmods"` Estimated hyper-parameters from
#' system models for births, deaths, ins, outs.
#' - `"datamods"` Estimated parameters from data models.
#' - `"data_population"` Input data on population.
#' - `"data_events"` Input data on births, deaths, ins, outs.
#'
#' @section Aggregating classification variables:
#'
#' [estimate_account()] constructs estimates that
#' are disaggregated by age, sex, cohort, and time.
#' Aggregated versions of these estimates
#' be obtained using the `collapse` argument. Setting
#' `collapse = "cohort"`, for instance,
#' yields estimates that are disaggregated only
#' by age, sex, and time.
#'
#' @inheritParams augment_population
#' @param what Component(s) to extract. Valid
#' values are `"population"`, `"events"`,
#' `"exposure"`, `"rates"`, `"sysmods"`', `"datamods"`,
#' "`data_population"`, and `"data_events"`.
#' @param collapse Classification variables
#' to aggregate over. One or more of `"age"`, `"sex"`,
#' `"cohort"`, and `"time"`. If collapse is `NULL`
#' (the default), then no collapsing is done.
#' @param na_rm Whether to replace `NA`s in result
#' with `0`s. Default is `FALSE`.
#' @param ... Not currently used.
#'
#' @returns
#' A [tibble][tibble::tibble-package],
#' or named list of tibbles.
#' If ex-post adjustments were needed to generate
#' non-negative population counts, then a tibble
#' called 'propn_adjust' is added to the result,
#' showing the proportion of draws that were adjusted,
#' by cohort and sex.
#'
#' Tibbles include:
#' - A list column with `n_draw` draws from the
#' distribution.
#' - Point estimates (means), marked `.fitted`.
#' - Lower and upper limits of credible intervals
#' specified by `width`, marked `.lower` and `.upper`.
#'
#' @seealso
#' - [augment_population()] Extract merged data and estimates for population.
#' - [augment_events()] Extract merged data and estimates for events.
#' - [augment_rates()] Extract merged priors and estimates for rates.
#' - [diagnostics()] Extract measures of model performance.
#'
#' @examples
#' ## use results from an existing model
#' results <- dpmaccount::results_greenland
#'
#' ## extract a single component
#' components(results, what = "population")
#'
#' ## extract two components
#' components(results, what = c("events", "rates"))
#'
#' ## population data is returned separately
#' ## from events data, since population data
#' ## refers to time points, and events data
#' ## to periods
#' components(results, what = c("data_population", "data_events"))
#'
#' ## results from data models
#' components(results, what = "datamods")
#' @export
components.dpmaccount_results <- function(object,
                                          what = "population",
                                          collapse = NULL,
                                          n_draw = 1000,
                                          width = 0.95,
                                          na_rm = FALSE,
                                          ...) {
  choices_what <- c(
    "population",
    "events",
    "exposure",
    "rates",
    "sysmods",
    "datamods",
    "data_events",
    "data_population"
  )
  uses_popn <- c(
    "population",
    "events",
    "exposure",
    "rates"
  )
  cohort <- object$cohort
  sex <- object$sex
  fitted <- object$fitted
  seed_list <- object$seed_list
  ## check inputs
  checkmate::assert_subset(what,
    choices = choices_what,
    empty.ok = FALSE
  )
  n_draw <- checkmate::assert_count(n_draw,
    positive = TRUE,
    coerce = TRUE
  )
  checkmate::assert_number(width,
    lower = 0,
    upper = 1
  )
  can_collapse_time <- (!("population" %in% what) &&
    !("data_population" %in% what))
  check_collapse(
    collapse = collapse,
    can_collapse_time = can_collapse_time
  )
  has_collapse <- !is.null(collapse)
  add_expose_for_collapse <- (has_collapse &&
    !("exposure" %in% what) &&
    (("rates" %in% what) || ("sysmods" %in% what)))
  if (add_expose_for_collapse) {
    what <- c(what, "exposure")
  }
  ## extract components
  results <- lapply(fitted,
    components_cohort,
    what = what,
    n_draw = n_draw,
    seed_list = seed_list
  )
  ## join cohort-level results together as tibbles
  ans <- vector(mode = "list", length = length(what))
  names(ans) <- what
  for (nm in what) {
    vals <- lapply(results, function(x) x[[nm]])
    nrows <- vapply(vals, nrow, 0L)
    vals <- do.call(rbind, vals)
    vals <- tibble::tibble(
      cohort = rep(cohort, times = nrows),
      sex = rep(sex, times = nrows),
      vals
    )
    ans[[nm]] <- vals
  }
  ## rearrange births so sex refers to child
  if ("events" %in% what) {
    ans[["events"]] <- pivot_sex_index_births(
      df = ans[["events"]],
      nm_bth = "births"
    )
  }
  if ("data_events" %in% what) {
    nm_bth <- get_nm_data_bth(fitted[[1L]])
    ans[["data_events"]] <- pivot_sex_index_births(
      df = ans[["data_events"]],
      nm_bth = nm_bth
    )
  }
  ## extract detailed exposure
  if ("exposure" %in% what) {
    exposure_detailed <- ans[["exposure"]][["exposure"]]
  }
  ## remove exposure if included in 'ans' but not in original 'what'
  if (add_expose_for_collapse) {
    ans <- ans[-match("exposure", names(ans))]
  }
  nms_ans <- names(ans)
  ## collapse where necessary
  if (has_collapse) {
    if ("population" %in% nms_ans) {
      ans[["population"]] <- aggregate_count(
        df = ans[["population"]],
        collapse = collapse,
        na_rm = na_rm
      )
    }
    if ("events" %in% nms_ans) {
      ans[["events"]] <- aggregate_count(
        df = ans[["events"]],
        collapse = collapse,
        na_rm = na_rm
      )
    }
    if ("exposure" %in% nms_ans) {
      ans[["exposure"]] <- aggregate_count(
        df = ans[["exposure"]],
        collapse = collapse,
        na_rm = na_rm
      )
    }
    if ("rates" %in% nms_ans) {
      ## convert rate for ins from events per year
      ## to events per triangle before adding up
      ans[["rates"]][["ins"]] <- lapply(ans[["rates"]][["ins"]], function(x) 0.5 * x)
      ans[["rates"]] <- aggregate_rate(
        df = ans[["rates"]],
        collapse = collapse,
        unweighted = "ins",
        exposure = exposure_detailed,
        na_rm = na_rm
      )
    }
    if ("sysmods" %in% nms_ans) {
      ## convert rate for ins from events per year
      ## to events per triangle before adding up
      ans[["sysmods"]][["ins.mean"]] <- 0.5 * ans[["sysmods"]][["ins.mean"]]
      ans[["sysmods"]] <- aggregate_rate(
        df = ans[["sysmods"]],
        collapse = collapse,
        unweighted = "ins.mean",
        exposure = exposure_detailed,
        na_rm = na_rm
      )
    }
    if ("data_population" %in% nms_ans) {
      ans[["data_population"]] <- aggregate_count(
        df = ans[["data_population"]],
        collapse = collapse,
        na_rm = na_rm
      )
    }
    if ("data_events" %in% nms_ans) {
      ans[["data_events"]] <- aggregate_count(
        df = ans[["data_events"]],
        collapse = collapse,
        na_rm = na_rm
      )
    }
  }
  ## add summaries
  if ("population" %in% nms_ans) {
    ans[["population"]] <- add_summaries(
      df = ans[["population"]],
      vname = "population",
      width = width
    )
  }
  if ("events" %in% nms_ans) {
    for (vname in c("ins", "outs")) {
      ans[["events"]] <- add_summaries(
        df = ans[["events"]],
        vname = vname,
        width = width
      )
    }
  }
  if ("rates" %in% nms_ans) {
    for (vname in c("births", "deaths", "ins", "outs")) {
      ans[["rates"]] <- add_summaries(
        df = ans[["rates"]],
        vname = vname,
        width = width
      )
    }
  }
  if ("datamods" %in% nms_ans) {
    if (nrow(ans[["datamods"]]) > 0L) {
      ans[["datamods"]] <- add_summaries(
        df = ans[["datamods"]],
        vname = "value",
        width = width
      )
    }
  }
  ## add diagnostic 'propn_adjusted'
  if (any(uses_popn %in% what)) {
    propn_adjusted <- vapply(results, function(x) x$propn_adjusted, 0)
    if (any(propn_adjusted > 0)) {
      propn_adjusted <- tibble::tibble(
        cohort = cohort,
        sex = sex,
        propn_adjusted = propn_adjusted
      )
      ans[["propn_adjusted"]] <- propn_adjusted
    }
  }
  ## unlist if only one element
  if (identical(length(ans), 1L)) {
    ans <- ans[[1L]]
  }
  ans
}


## 'diagnostics' --------------------------------------------------------------

#' Diagnostics on estimation process
#'
#' Get diagnostics on the process of
#' estimating an account. Separate summaries
#' are presented for each combination of
#' cohort and sex, since estimation is carried
#' out separately for each of these combinations.
#'
#' @section Warning:
#' The value `logpost` gives the value of the
#' *unnormalised* log posterior at the maximum.
#' The unnormalised log posterior is only defined
#' up to a constant, and cannot be compared
#' across cohorts. (Comparing the same cohort across
#' models may be meaningful, depending on the models.)
#'
#' @param object An object of class `"dpmaccount_results"`
#'
#' @returns A tibble with the following columns:
#' - `cohort` Birth cohort.
#' - `sex` Sex or gender.
#' - `nlminb_res` Whether the optimiser [nlminb()]
#' returned results.
#' - `nlminb_conv` Whether `nlminb()` reached
#' convergence (given that `nlminb_res` is `TRUE`).
#' - `nlminb_msg` Message returned by `nlminb()`.
#' - `logpost` The value for the unnormalised log-posterior
#' at the maximum.
#' - `var_nona` Whether the variance matrix
#' originally returned by `nlminb()` had no NAs or Infs
#' (given that `nlminb_cov` is `TRUE`).
#' - `var_pos` Whether the variance matrix
#' originally returned by `nlminb()` is positive
#' definite (given that `var_nonna` is `TRUE`).
#' - `var_nearpos` Whether a non-positive definite
#' matrix could be corrected via
#' [Matrix::nearPD()] (given that `val_pos` is `FALSE`).
#' - `success` Whether the estimation process succeeded.
#' `TRUE` if and only if `var_pos` or `var_nearpos` is `TRUE`.
#'
#' @seealso
#' - \code{\link[=components.dpmaccount_results]{components()}}
#' Extract estimates, inputs.
#'
#' @examples
#' ## use results from an existing model
#' results <- dpmaccount::results_greenland
#'
#' ## get diagnostics
#' diagnostics(results)
#' @export
diagnostics <- function(object) {
  UseMethod("diagnostics")
}

#' @export
diagnostics.dpmaccount_results <- function(object) {
  fitted <- object$fitted
  cohort <- object$cohort
  sex <- object$sex
  n <- length(fitted)
  nlminb_res <- vapply(fitted, function(x) !is.null(x$val_nlminb), TRUE)
  nlminb_conv <- rep(NA, times = n)
  nlminb_conv[nlminb_res] <- vapply(
    fitted[nlminb_res],
    function(x) x$nlminb_conv,
    TRUE
  )
  nlminb_msg <- rep(NA_character_, times = n)
  nlminb_msg[nlminb_res] <- vapply(
    fitted[nlminb_res],
    function(x) x$val_nlminb$message,
    ""
  )
  logpost <- rep(NA_real_, times = n)
  logpost[nlminb_res] <- vapply(
    fitted[nlminb_res],
    function(x) -1 * x$val_nlminb$objective,
    0
  )
  var_nona <- vapply(
    fitted,
    function(x) x$var_nona,
    TRUE
  )
  var_pos <- vapply(
    fitted,
    function(x) x$var_pos,
    TRUE
  )
  var_nearpos <- vapply(
    fitted,
    function(x) x$var_nearpos,
    TRUE
  )
  success <- (!is.na(var_pos) & var_pos) | (!is.na(var_nearpos) & var_nearpos)
  tibble::tibble(
    cohort = cohort,
    sex = sex,
    nlminb_res = nlminb_res,
    nlminb_conv = nlminb_conv,
    nlminb_msg = nlminb_msg,
    logpost = logpost,
    var_nona = var_nona,
    var_pos = var_pos,
    var_nearpos = var_nearpos,
    success = success
  )
}


## 'print' --------------------------------------------------------------------

#' @export
print.dpmaccount_results <- function(x, ...) {
  ## extract info
  datamods_stk <- x$fitted[[1L]]$datamods_stk
  datamods_ins <- x$fitted[[1]]$datamods_ins
  datamods_outs <- x$fitted[[1L]]$datamods_outs
  diag <- diagnostics(x)
  n_cohort <- nrow(diag)
  n_success <- sum(diag$success)
  ## make strings
  str_title <- sprintf(
    "Object of class \"%s\"\n",
    class(x)[[1L]]
  )
  str_sys <- "   --- system models ---\n"
  str_sys_bth <- "    births ~ Poisson(rate * exposure)\n"
  str_sys_dth <- "    deaths ~ Poisson(rate * exposure)\n"
  str_sys_ins <- "       ins ~ Poisson(rate)\n"
  str_sys_outs <- "      outs ~ Poisson(rate * exposure)\n"
  str_data <- "   --- data models ---\n"
  str_data_popn <- .mapply(make_str,
    dots = list(
      x = datamods_stk,
      nm_data = names(datamods_stk)
    ),
    MoreArgs = list(nm_series = "population")
  )
  str_data_ins <- .mapply(make_str,
    dots = list(
      x = datamods_ins,
      nm_data = names(datamods_ins)
    ),
    MoreArgs = list(nm_series = "ins")
  )
  str_data_outs <- .mapply(make_str,
    dots = list(
      x = datamods_outs,
      nm_data = names(datamods_outs)
    ),
    MoreArgs = list(nm_series = "outs")
  )
  str_success <- sprintf(
    "  Estimation succeeded in %d out of %d cohorts\n",
    n_success, n_cohort
  )
  ## print
  cat(str_title)
  cat("\n")
  cat(str_sys)
  cat("\n")
  for (str in c(str_sys_bth, str_sys_dth, str_sys_ins, str_sys_outs)) {
    cat(str)
  }
  cat("\n")
  cat(str_data)
  cat("\n")
  for (str in c(str_data_popn, str_data_ins, str_data_outs)) {
    cat(str)
  }
  cat("\n")
  cat(str_success)
  cat("\n")
  ## return
  invisible(x)
}


## 'summary' ------------------------------------------------------------------

#' Summarise results from a call to estimate_account
#'
#' Summary method for an object of class
#' `"dpmaccount_results"`, typically created by
#' a call to [estimate_account()]. The summary consists
#' of diagnostic information, plus estimated counts
#' and rates for each time period (with no age,
#' sex, or cohort detail.)
#'
#' @param object Object of class `"dpmaccount_results"`.
#' @param ... Currently ignored.
#'
#' @returns A named list, invisibly.
#'
#' @rdname summary.dpmaccount_results
#' @export
summary.dpmaccount_results <- function(object, ...) {
  ## extract info
  l <- components(object,
    what = c("population", "events", "rates"),
    collapse = c("age", "sex", "cohort"),
    na_rm = TRUE
  )
  population <- with(
    l$population,
    data.frame(
      time = time,
      population = round(population.fitted)
    )
  )
  events <- with(
    l$events,
    data.frame(
      time = time,
      births = round(births),
      deaths = round(deaths),
      ins = round(ins.fitted),
      outs = round(outs.fitted)
    )
  )
  rates <- with(
    l$rates,
    data.frame(
      time = time,
      births = signif(births.fitted, 3),
      deaths = signif(deaths.fitted, 3),
      ins = signif(ins.fitted, 3),
      outs = signif(outs.fitted, 3)
    )
  )
  diag <- diagnostics(object)
  n_cohort <- nrow(diag)
  n_success <- sum(diag$success)
  ans <- list(
    population = population,
    events = events,
    rates = rates,
    n_cohort = n_cohort,
    n_success = n_success
  )
  class(ans) <- paste0(class(object)[[1L]], "_summary")
  ans
}


#' @export
print.dpmaccount_results_summary <- function(x, ...) {
  ## make strings
  str_title <- sprintf(
    "Summary of object of class \"%s\"\n",
    gsub("_summary", "", class(x)[[1L]])
  )
  str_success <- sprintf(
    " Estimation succeeded in %d out of %d cohorts\n",
    x$n_success, x$n_cohort
  )
  str_popn <- " --- population ---\n"
  str_events <- " --- events ---\n"
  str_rates <- " --- rates ---\n"
  ## print
  cat(str_title)
  cat("\n")
  cat(str_success)
  cat("\n")
  cat(str_popn)
  cat("\n")
  print(x$population)
  cat("\n")
  cat(str_events)
  cat("\n")
  print(x$events)
  cat("\n")
  cat(str_rates)
  cat("\n")
  print(x$rates)
  cat("\n")
  ## return
  invisible(x)
}
