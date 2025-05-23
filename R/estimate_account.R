#' Estimate a demographic account
#'
#' Estimate consistent counts of population,
#' births, deaths, and migration, and associated
#' quantities such as demographic rates.
#' The main inputs are
#' - a set of system models describing regularities
#' in underlying birth, death, and migration rates
#' - a set of data models, describing the relationship
#' between true and reported counts of population and
#' events.
#'
#' At present the choice of data models is:
#' - [datamod_exact()], which is used only
#' with births and deaths
#' - [datamod_norm()], which assumes that errors
#' are normally-distributed, with known variances.
#'
#' More data models will be added in future.
#'
#' @param sysmods List of system models
#' for births, deaths, inward movements,
#' and outward movements. Specified using
#' function [sysmod()].
#' @param datamods List of data models for
#' population, births, deaths, inward movements,
#' and outward movements.
#' @param keep_adfun Whether to keep the functions
#' created internally by `TMB::MakeADFun()`.
#' For expert use only. Defaults to `FALSE`.
#'
#' @returns An object of class `"dpmaccount_results"`.
#'
#' @seealso
#' Setting up model:
#' - [sysmod()] System models for rates.
#' - [datamod_exact()] Data model for error-free data.
#' - [datamod_norm()] Data model assuming normally-distributed
#' measurement and/or sampling errors.
#'
#' Extracting results:
#' - [augment_population()] Data and estimates for population.
#' - [augment_events()] Data and estimates for events.
#' - [augment_rates()] priors and estimates for rates.
#' #' - \code{\link[=components.dpmaccount_results]{components()}}
#' Lower-level access to data and estiamtes.
#' - [diagnostics()] Measures of model performance
#'
#' @examples
#' ## create system models
#' sysmod_births <- sysmod(
#'   mean = gl_sysmod_mean_births,
#'   disp = 0.2,
#'   nm_series = "births"
#' )
#' sysmod_deaths <- sysmod(
#'   mean = gl_sysmod_mean_deaths,
#'   disp = 0.2,
#'   nm_series = "deaths"
#' )
#' sysmod_ins <- sysmod(
#'   mean = gl_sysmod_mean_immig,
#'   disp = 0.2,
#'   nm_series = "ins"
#' )
#' sysmod_outs <- sysmod(
#'   mean = gl_sysmod_mean_emig,
#'   disp = 0.2,
#'   nm_series = "outs"
#' )
#' sysmods <- list(
#'   sysmod_births,
#'   sysmod_deaths,
#'   sysmod_ins,
#'   sysmod_outs
#' )
#'
#' ## create data models
#' datamod_popn <- datamod_norm(
#'   data = gl_report_popn,
#'   sd = gl_cover_sd_popn,
#'   nm_series = "population"
#' )
#' datamod_births <- datamod_exact(
#'   data = gl_report_births,
#'   nm_series = "births"
#' )
#' datamod_deaths <- datamod_exact(
#'   data = gl_report_deaths,
#'   nm_series = "deaths"
#' )
#' datamod_ins <- datamod_norm(
#'   data = gl_report_immig,
#'   sd = gl_cover_sd_immig,
#'   nm_series = "ins"
#' )
#' datamod_outs <- datamod_norm(
#'   data = gl_report_emig,
#'   sd = gl_cover_sd_emig,
#'   nm_series = "outs"
#' )
#' datamods <- list(
#'   datamod_popn = datamod_popn,
#'   datamod_births = datamod_births,
#'   datamod_deaths = datamod_deaths,
#'   datamod_ins = datamod_ins,
#'   datamod_outs = datamod_outs
#' )
#'
#' ## do estimation
#' results <- estimate_account(
#'   sysmods = sysmods,
#'   datamods = datamods
#' )
#'
#' ## object holding estimation results
#' results
#'
#' ## high-level summary
#' summary(results)
#' @export
estimate_account <- function(sysmods,
                             datamods = list(),
                             keep_adfun = FALSE) {
  ## check inputs
  check_sysmods(sysmods)
  check_datamods(datamods)
  check_mods(
    sysmods = sysmods,
    datamods = datamods
  )
  checkmate::assert_flag(keep_adfun)
  ## make 'classif_vars' data frame, holding all
  ## levels of all classification variables: at
  ## present get these from the data for deaths
  classif_vars <- make_classif_vars(datamods)
  ## make data frames classified by cohort and sex
  ## holding cohort-level counts, system models,
  ## and data models
  costkinit_df <- make_costkinit_df(
    datamods = datamods,
    classif_vars = classif_vars
  )
  cocounts_df <- make_cobthdth_df(datamods)
  cosysmods_df <- make_cosysmods_df(
    sysmods = sysmods,
    classif_vars = classif_vars
  )
  codatamods_df <- make_codatamods_df(
    datamods = datamods,
    classif_vars = classif_vars
  )
  ## combine into single data frame
  all_dfs <- list(
    costkinit_df,
    cocounts_df,
    cosysmods_df,
    codatamods_df
  )
  df <- Reduce(merge, all_dfs)
  df <- tibble::tibble(df)
  ## create "comod" (cohort model) objects
  comod <- .mapply(new_comod, dots = df, MoreArgs = list())
  ## fit models
  fitted <- lapply(comod, fit, keep_adfun = keep_adfun)
  ## return results
  if (keep_adfun) {
    adfun <- lapply(fitted, function(x) x$adfun)
    fitted <- lapply(fitted, function(x) {
      x$adfun <- NULL
      x
    })
    new_dpmaccount_results(
      cohort = df$cohort,
      sex = df$sex,
      fitted = fitted,
      adfun = adfun
    )
  } else {
    new_dpmaccount_results(
      cohort = df$cohort,
      sex = df$sex,
      fitted = fitted
    )
  }
}
