new_dpmaccount_results <- function(cohort, sex, fitted, adfun = NULL) {
  ans <- list(
    cohort = cohort,
    sex = sex,
    fitted = fitted
  )
  if (!is.null(adfun)) {
    ans <- c(ans, list(adfun = adfun))
  }
  class(ans) <- "dpmaccount_results"
  ans
}
