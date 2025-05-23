#' Combines cohort and sex information with output from fitted
#' TMB object and assigns into dpmaccount_results object
#'
#' @param cohort The relevant cohort of the fitted TMB object
#' @param sex The relevant sex of the fitted TMB object
#' @param fitted Fitted values after TMB optimisation
#' @param adfun Automatic differentiation function object
#'
#' @return dpmaccount_results object
#'
#' @noRd
new_dpmaccount_results <- function(cohort, sex, fitted, adfun = NULL, seed_list) {
  ans <- list(
    cohort = cohort,
    sex = sex,
    fitted = fitted,
    seed_list = seed_list
  )
  if (!is.null(adfun)) {
    ans <- c(ans, list(adfun = adfun))
  }
  class(ans) <- "dpmaccount_results"
  ans
}
