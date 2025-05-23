## HAS_TESTS
#' Search across single dataset for measurements
#'
#' Search across multiple columns in a single dataset
#' for measures for each
#' combination of the classification variables.
#' The measures are values reported in the datasets that we are using as proxies
#' for the underlying demographic counts (eg stocks of people).
#' Assume columns ordered from left to right
#' by reliability.
#'
#' @param df A data frame
#' @param nm Name of the measurement variable
#' in the result.
#'
#' @return A data frame with classification variables
#' and best estimate of measurement variable.
#'
#' @noRd
get_best_value_one <- function(df, nm) {
  nms_df <- names(df)
  nms_classif_vars <- c("age", "sex", "cohort", "time")
  nms_classif_obs <- intersect(nms_df, nms_classif_vars)
  nms_measure_vars <- setdiff(nms_df, nms_classif_vars)
  measure_vars <- df[nms_measure_vars]
  ans <- measure_vars[[1L]]
  n <- ncol(measure_vars)
  if (n > 1L) {
    for (i in seq.int(from = 2L, to = n)) {
      is_na <- is.na(ans)
      if (sum(is_na) == 0L) {
        break
      }
      val <- measure_vars[[i]]
      ans[is_na] <- val[is_na]
    }
  }
  ans <- cbind(df[nms_classif_obs], ans)
  names(ans)[length(ans)] <- nm
  ans
}


## HAS_TESTS
#' Calculate exposure from raw population data
#'
#' Calculate exposure from input data on population,
#' then align to events data.
#'
#' Population and events data are assumed to have
#' the same classification variables (though some different
#' levels).
#'
#' @param popn_df Data frame with population data.
#' Each measurement column is one dataset, with data
#' ordered by reliability: left-most columns are
#' most reliable.
#' @param events_df Data frame with events data.
#'
#' @returns A vector of exposures, ordered to line
#' up with 'events_df'.
#'
#' @noRd
make_exposure_direct <- function(popn_df, events_df) {
  nms_df <- names(popn_df)
  nms_classif_vars <- c("age", "sex", "cohort", "time")
  nms_classif_obs <- intersect(nms_classif_vars, nms_df)
  nms_classif_nocohort <- setdiff(nms_classif_obs, "cohort")
  has_age <- "age" %in% nms_classif_obs
  has_sex <- "sex" %in% nms_classif_obs
  has_cohort <- "cohort" %in% nms_classif_obs
  has_time <- "time" %in% nms_classif_obs
  has_lexis <- has_age && has_cohort && has_time
  popn <- get_best_value_one(
    df = popn_df,
    nm = "population"
  )
  nms_by <- NULL
  if (has_age) {
    nms_by <- c(nms_by, "age")
  }
  if (has_sex) {
    nms_by <- c(nms_by, "sex")
  }
  if (has_cohort && !has_age) {
    nms_by <- c(nms_by, "cohort")
  }
  if (is.null(by)) {
    popn <- list(popn)
  } else {
    popn <- split(popn, popn[nms_by])
  }
  exposure <- lapply(popn,
    make_exposure_direct_inner,
    has_lexis = has_lexis
  )
  exposure <- do.call(rbind, exposure)
  nms_classif_expose <- c(nms_by, "time")
  exposure <- exposure[c(nms_classif_expose, "exposure")]
  paste_dot <- function(...) paste(..., sep = ".")
  key_events <- do.call(paste_dot, events_df[nms_classif_expose])
  key_exposure <- do.call(paste_dot, exposure[nms_classif_expose])
  i <- match(key_events, key_exposure)
  ## return vector of exposures
  exposure$exposure[i]
}


## HAS_TESTS
#' Calculate exposure for one sub-population
#'
#' If Lexis triangles are being used,
#' divide result by two.
#'
#' @param df Data frame with
#' - population variable
#' - one or more of age, time, or cohort
#' - optional other variables, which are ignored
#' @param has_lexis Whether exposures
#' are to be used with Lexis triangles
#'
#' @returns A data frame with the original
#' variables, except that the 'population'
#' variable is replaced with an 'exposure' variable.
#' There is also one less row.
#'
#' @noRd
make_exposure_direct_inner <- function(df, has_lexis) {
  nms_df <- names(df)
  if ("time" %in% nms_df) {
    ord <- order(df$time)
  } else if ("age" %in% nms_df) {
    ord <- order(df$age)
  } else {
    ord <- order(df$cohort)
  }
  df <- df[ord, , drop = FALSE]
  popn_start <- df$population[-nrow(df)]
  popn_end <- df$population[-1L]
  mult <- if (has_lexis) 0.25 else 0.5
  exposure <- mult * (popn_start + popn_end)
  ans <- df[-1L, , drop = FALSE]
  ans <- ans[-match("population", names(ans))]
  ans$exposure <- exposure
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Convert births data from sex-of-child format
#' to sex-of-parent format
#'
#' Given a data frame where the 'sex' variable
#' refers to the sex of the child, and birth
#' counts are numeric vectors, convert to the
#' format where the 'sex' variable refers to the
#' sex of the parent (assumed to be "Female"),
#' and where the sex of births is not recorded.
#'
#' @param df A data frame
#' @param nm_bth The name of the variable
#' holding births counts
#'
#' @returns A data frame
#'
#' @seealso pivot_sex_index_births for the
#' approximate inverse
#'
#' @noRd
unpivot_sex_index_births <- function(df, nm_bth) {
  nms_classif_vars <- c("age", "sex", "cohort", "time")
  nms_df <- names(df)
  if (!("sex" %in% nms_df)) {
    stop(
      gettextf(
        "data frame does not contain '%s' variable",
        "sex"
      ),
      call. = FALSE
    )
  }
  nms_classif_obs <- intersect(nms_classif_vars, nms_df)
  nms_classif_nosex <- setdiff(nms_classif_obs, "sex")
  df_ag <- stats::aggregate(
    df[nm_bth],
    df[nms_classif_nosex],
    sum
  )
  df_ag$sex <- "Female"
  df[[nm_bth]] <- NA_real_
  paste_dot <- function(...) paste(..., sep = ".")
  key_df <- do.call(paste_dot, df[nms_classif_obs])
  key_df_ag <- do.call(paste_dot, df_ag[nms_classif_obs])
  i_ag <- match(key_df, key_df_ag)
  df[[nm_bth]] <- df_ag[[nm_bth]][i_ag]
  df
}
