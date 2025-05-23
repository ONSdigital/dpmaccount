## Construct objects of class "dpmaccount_comod"

## NO_TESTS
#' Make an object of class "dpmaccount_comod"
#'
#' Make an object of class "dpmaccount_comod"
#' that holds information needed to estimate values
#' for a cohort, and, after fitting, the estimates
#' themselves. "comod" is short for "cohort model".
#'
#' @param cohort Birth cohort. An integer.
#' @param sex Sex. A string.
#' @param is_new_cohort Whether cohort born during
#' estimation period
#' @param prior_stk_init Prior for initial stock. A named list.
#' @param count_bthdth Counts of births and deaths. A data frame.
#' @param sysmod_bth System model for births. A data frame.
#' @param sysmod_dth System model for deaths. A data frame.
#' @param sysmod_ins System model for inward moves. A data frame.
#' @param sysmod_outs System model for outward moves. A data frame.
#' @param datamods_stk Data models for stock. List.
#' @param datamods_ins Data models for inward moves. List.
#' @param datamods_outs Data models for outward moves. List.
#'
#' @returns An object of class "dpmaccount_comod"
#'
#' @noRd
new_comod <- function(cohort,
                      sex,
                      is_new_cohort,
                      prior_stk_init,
                      count_bthdth,
                      sysmod_bth,
                      sysmod_dth,
                      sysmod_ins,
                      sysmod_outs,
                      datamods_stk,
                      datamods_ins,
                      datamods_outs) {
  ans <- list(
    cohort = cohort,
    sex = sex,
    is_new_cohort = is_new_cohort,
    prior_stk_init = prior_stk_init,
    count_bthdth = count_bthdth,
    sysmod_bth = sysmod_bth,
    sysmod_dth = sysmod_dth,
    sysmod_ins = sysmod_ins,
    sysmod_outs = sysmod_outs,
    datamods_stk = datamods_stk,
    datamods_ins = datamods_ins,
    datamods_outs = datamods_outs,
    mean = NULL,
    var = NULL
  )
  class(ans) <- "dpmaccount_comod"
  ans
}
