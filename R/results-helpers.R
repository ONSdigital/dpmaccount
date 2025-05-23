## 'augment_population' -------------------------------------------------------

## HAS_TESTS
#' Extract merged data and estimates for population
#'
#' Extract data and estimates for population
#' from a results object
#' returned by [estimate_account()].
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
#' Adding up population
#' counts at different times does not make sense,
#' so `augment_population()` (unlike [augment_events()])
#' does not allow `collapse` to include `"time"`.
#'
#' @param object An object of class `"dpmaccount_results"`,
#' created by [estimate_account()].
#' @param collapse Classification variables
#' to aggregate over: one or more of `"age"`, `"sex"`,
#' and `"cohort"`. If collapse is `NULL`
#' (the default), no collapsing is done.
#' @param n_draw Number of draws in posterior
#' samples. Default is 1000.
#' @param width A number between 0 and 1 giving
#' the width of credible intervals. Default is 0.95.
#' @param na_rm Whether to replace `NA`s in result
#' with `0`s. Default is `FALSE`.
#'
#' @returns A [tibble][tibble::tibble-package] with:
#' - Reported values for population from input datasets.
#' - A list column with `n_draw` draws from the posterior
#' distribution for population.
#' - Point estimates (posterior means), marked `.fitted`.
#' - Lower and upper limits of credible intervals
#' specified by `width`, marked `.lower` and `.upper`.
#'
#' @seealso
#' - [augment_events()] Extract merged data and estimates for events.
#' - [augment_rates()] Extract merged priors and estimates for rates.
#' - \code{\link[=components.dpmaccount_results]{components()}}
#' Extract estimates, inputs.
#' - [diagnostics()] Extract measures of model performance.
#'
#' @examples
#' ## use results from an existing model
#' results <- dpmaccount::results_greenland
#'
#' ## extract population counts based on
#' ## complete classification with
#' ## age, sex, cohort, and time
#' augment_population(results)
#'
#' ## extract population counts based on
#' ## simpler classification with
#' ## sex and time
#' augment_population(results, collapse = c("age", "cohort"))
#' @export
augment_population <- function(object,
                               collapse = NULL,
                               n_draw = 1000,
                               width = 0.95,
                               na_rm = FALSE) {
  components <- components(object,
    what = c("population", "data_population"),
    collapse = collapse,
    n_draw = n_draw,
    width = width,
    na_rm = na_rm
  )
  data <- components$data_population
  population <- components$population
  ans <- merge(data, population, all.y = TRUE)
  ans <- tibble::as_tibble(ans)
  ans
}


## 'augment_events' -----------------------------------------------------------

#' Extract merged data and estimates for events
#'
#' Extract merged input data and estimates for
#' counts of events from a results object
#' returned by [estimate_account()].
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
#' @param collapse Classification variables
#' to aggregate over: one or more of `"age"`, `"sex"`,
#' `"cohort"`, and `"time"`. If collapse is `NULL`
#' (the default), no collapsing is done.
#'
#' @returns A [tibble][tibble::tibble-package] with:
#' - Reported values from input datasets.
#' - Lists columns with `n_draw` draws from posterior
#' distributions
#' - Point estimates (posterior means), marked `.fitted`.
#' - Lower and upper limits of credible intervals
#' specified by `width`, marked `.lower` and `.upper`.
#'
#' @seealso
#' - [augment_population()] Extract merged data and estimates for population.
#' - [augment_rates()] Extract merged priors and estimates for rates.
#' - \code{\link[=components.dpmaccount_results]{components()}}
#' Extract estimates, inputs.
#' - [diagnostics()] Extract measures of model performance.
#'
#' @examples
#' ## use results from an existing model
#' results <- dpmaccount::results_greenland
#'
#' ## extract counts of events using
#' ## complete classsification with
#' ## age, sex, cohort, and time
#' augment_events(results)
#'
#' ## extract counts of events using
#' ## simpler classification with
#' ## sex and time
#' augment_events(results, collapse = c("age", "cohort"))
#' @export
augment_events <- function(object,
                           collapse = NULL,
                           n_draw = 1000,
                           width = 0.95,
                           na_rm = FALSE) {
  components <- components(object,
    what = c("events", "data_events"),
    collapse = collapse,
    n_draw = n_draw,
    width = width,
    na_rm = na_rm
  )
  data <- components$data_events
  events <- components$events
  ans <- merge(data, events, all.y = TRUE)
  ans <- tibble::as_tibble(ans)
  ans
}


## 'augment_rates' ------------------------------------------------------------

#' Extract merged priors and estimates for demographic rates
#'
#' Given a results object returned by [estimate_account()],
#' extract three types of demographic rates:
#' - Prior values derived from system models.
#' - Posterior values derived from the combination of
#' system models, data models, and data.
#' - Direct estimates, derived from data only.
#'
#' The rates are defined as follows:
#' - Births: expected births per person-year
#' - Deaths: expected deaths per person-year
#' - Ins: expected inward movements per year (*not per person-year*)
#' - Outs: expected outward movements per person-year
#'
#' Estimates for each series include:
#'
#' - A list column with `n_draw` draws from posterior
#' distributions.
#' - Point estimates (posterior means), marked `.fitted`.
#' - Lower and upper limits of credible intervals
#' specified by `width`, marked `.lower` and `.upper`.
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
#' `augment_rates()` does not allow `collapse` to
#' include `"time"`. (Information on time is
#' needed for direct estimates.)
#'
#' @inheritParams augment_population
#' @param collapse Classification variables
#' to aggregate over: one or more of `"age"`, `"sex"`,
#' and `"cohort"`. If collapse is `NULL`
#' (the default), no collapsing is done.
#'
#' @returns A [tibble][tibble::tibble-package] with four
#' groups of variables
#' - Classification variables. One or more of `"age"`, `"sex",
#' `"cohort", and `"time"`
#' - Draws from the prior distributions for rates,
#' marked `prior`.
#' - Draws from the posterior distribution for rates,
#' marked `post`.
#' - Direct estimates of rates, obtained from input data
#' with no modelling.
#'
#' Prior and posterior values include:
#' - A list column with `n_draw` draws from the
#' distribution.
#' - Point estimates (means), marked `.fitted`.
#' - Lower and upper limits of credible intervals
#' specified by `width`, marked `.lower` and `.upper`.
#'
#' @seealso
#' - [augment_population()] Extract merged data and estimates
#' for population.
#' - [augment_events()] Extract merged data and estimates for events.
#' - \code{\link[=components.dpmaccount_results]{components()}}
#' Extract estimates, inputs.
#' - [diagnostics()] Extract measures of model performance.
#'
#' @examples
#' ## use results from an existing model
#' results <- dpmaccount::results_greenland
#'
#' ## extract rates based on
#' ## complete classsification with
#' ## age, sex, cohort, and time
#' augment_rates(results)
#'
#' ## extract rates based on
#' ## simpler classification with
#' ## sex and time
#' augment_rates(results, collapse = c("age", "cohort"))
#' @export
augment_rates <- function(object,
                          collapse = NULL,
                          n_draw = 1000,
                          width = 0.95,
                          na_rm = FALSE) {
  nms_classif_vars <- c("age", "sex", "cohort", "time")
  nms_events <- c("births", "deaths", "ins", "outs")
  fitted1 <- object$fitted[[1L]]
  components <- components(object,
    collapse = collapse,
    what = c(
      "sysmods",
      "rates",
      "data_population",
      "data_events"
    ),
    n_draw = n_draw,
    width = width,
    na_rm = na_rm
  )
  sysmods <- components$sysmods
  rates <- components$rates
  data_popn <- components$data_population
  data_events <- components$data_events
  nms_classif_obs <- intersect(nms_classif_vars, names(rates))
  ## calculate priors
  draw_prior_rate <- function(mean, disp) {
    if (anyNA(mean)) {
      rep(NA_real_, times = n_draw)
    } else {
      stats::rgamma(n = n_draw, shape = 1 / disp, scale = mean * disp)
    }
  }
  prior <- sysmods[nms_classif_obs]
  for (nm in nms_events) {
    nm_rate <- paste0("prior.", nm)
    nm_mean <- paste0(nm, ".mean")
    nm_disp <- paste0(nm, ".disp")
    mean <- sysmods[[nm_mean]]
    disp <- sysmods[[nm_disp]]
    prior[[nm_rate]] <- .mapply(draw_prior_rate,
      dots = list(
        mean = mean,
        disp = disp
      ),
      MoreArgs = list()
    )
    prior <- add_summaries(prior,
      vname = nm_rate,
      width = width
    )
  }
  ## format posterior
  post <- rates
  is_nonclassif <- !(names(post) %in% nms_classif_vars)
  names(post)[is_nonclassif] <- paste0("post.", names(post)[is_nonclassif])
  ## calculate direct
  nm_data_bth <- get_nm_data_bth(fitted1)
  if ("sex" %in% nms_classif_obs) {
    data_events <- unpivot_sex_index_births(
      df = data_events,
      nm_bth = nm_data_bth
    )
  }
  exposure <- make_exposure_direct(
    popn_df = data_popn,
    events_df = data_events
  )
  direct <- data_events[nms_classif_obs]
  pd <- function(x) paste0("direct.", x)
  direct[[pd(nm_data_bth)]] <- data_events[[nm_data_bth]] / exposure
  nm_data_dth <- get_nm_data_dth(fitted1)
  direct[[pd(nm_data_dth)]] <- data_events[[nm_data_dth]] / exposure
  nms_data_ins <- names(fitted1$datamods_ins)
  for (nm in nms_data_ins) {
    direct[[pd(nm)]] <- data_events[[nm]]
  }
  nms_data_outs <- names(fitted1$datamods_outs)
  for (nm in nms_data_outs) {
    direct[[pd(nm)]] <- data_events[[nm]] / exposure
  }
  ## combine and return
  ans <- merge(direct, prior, by = nms_classif_obs)
  ans <- merge(ans, post, by = nms_classif_obs)
  ans <- tibble::as_tibble(ans)
  ans
}


## 'increments' ---------------------------------------------------------------

## HAS_TESTS
#' Calculate annual increments implied by data
#'
#' Calculate annual increments in population size
#' implied by input data on stocks and flows.
#'
#' @param object An object of class `"dpmaccount_results"`,
#' typically created by a call to [estimate_account()].
#'
#' @returns A tibble.
#'
#' @examples
#' results_greenland |>
#'   increments()
#' @export
increments <- function(object) {
  cohort <- object$cohort
  sex <- object$sex
  fitted <- object$fitted
  data <- lapply(fitted, increments_comod)
  n <- vapply(data, nrow, 0L)
  cohort <- rep(cohort, times = n)
  sex <- rep(sex, times = n)
  data <- do.call(rbind, data)
  tibble::tibble(
    cohort,
    sex,
    data
  )
}
