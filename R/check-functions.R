## HAS_TESTS
#' Check that an age variable is valid
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_age <- function(df, nm_df) {
  age <- df[["age"]]
  val <- checkmate::check_integerish(age,
    any.missing = FALSE,
    lower = 0L
  )
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with variable '%s' in data frame '%s' : %s",
        "age",
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Test whether an age variable has complete set of levels
#'
#' Used with function 'check_df_sysmod' (which requires
#' complete levels), but not with functions 'check_df_data'
#' or 'check_df_dataconst' (which do not).
#'
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#' @param is_births Whether the data frame is
#' describing birth rates or counts.
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_age_complete <- function(df, nm_df, is_births) {
  age <- df[["age"]]
  ## unless births, minimum age is 0
  age_min <- min(age)
  if (!is_births && (age_min != 0L)) {
    stop(
      gettextf(
        "minimum value for '%s' in data frame '%s' is %d",
        "age",
        nm_df,
        age_min
      ),
      call. = FALSE
    )
  }
  ## has all levels between minimum and maximum
  age_max <- max(age)
  complete_age <- seq.int(from = age_min, to = age_max)
  is_found_age <- complete_age %in% age
  i_not_found_age <- match(FALSE, is_found_age, nomatch = 0L)
  if (i_not_found_age > 0L) {
    stop(
      gettextf(
        "'%s' variable in data frame '%s' is missing value '%d'",
        "age",
        nm_df,
        complete_age[[i_not_found_age]]
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Put 'nm_series' into expected form
#'
#' @param nm_series Name of the demographic series
#'
#' @returns A string
#'
#' @noRd
check_and_tidy_nm_series <- function(nm_series) {
  choices <- c(
    "population",
    "births",
    "deaths",
    "ins",
    "outs"
  )
  match.arg(nm_series, choices = choices)
}


## HAS_TESTS
#' Check that classification variables include
#' all possible combinations
#'
#' Used with function 'check_df_sysmod' (which requires
#' complete levels), but not with functions 'check_df_data'
#' or 'check_df_dataconst' (which do not).
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_classif_complete <- function(df, nm_df) {
  nms_df <- names(df)
  nms_age_sex_time <- intersect(nms_df, c("age", "sex", "time"))
  age_sex_time <- df[nms_age_sex_time]
  n_unique <- function(x) length(unique(x))
  n_row_expected <- prod(vapply(age_sex_time, n_unique, 1L))
  has_age_cohort_time <- all(c("age", "cohort", "time") %in% nms_df)
  if (has_age_cohort_time) { ## Lexis triangles
    n_row_expected <- 2 * n_row_expected
  }
  too_few <- nrow(df) < n_row_expected
  if (too_few) {
    stop(
      gettextf(
        paste(
          "data frame '%s' does not include all possible",
          "combinations of classification variables"
        ),
        nm_df
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that classification variables have
#' no duplicate rows
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_classif_no_dup <- function(df, nm_df) {
  nms_classif_full <- c("age", "sex", "cohort", "time")
  nms_df <- names(df)
  nms_classif_vars <- intersect(nms_df, nms_classif_full)
  classif_vars <- df[nms_classif_vars]
  is_dup <- duplicated(classif_vars)
  i_dup <- match(TRUE, is_dup, nomatch = 0L)
  if (i_dup > 0L) {
    vals <- classif_vars[i_dup, ]
    vals <- sapply(vals, as.character)
    vals <- sprintf("'%s'", vals)
    vals <- paste(vals, collapse = ", ")
    stop(
      gettextf(
        paste(
          "classification variables for data frame '%s' have",
          "two rows with the following values : %s"
        ),
        nm_df,
        vals
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that a cohort variable is valid
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_cohort <- function(df, nm_df) {
  cohort <- df[["cohort"]]
  val <- checkmate::check_integerish(cohort,
    any.missing = FALSE
  )
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with variable '%s' in data frame '%s' : %s",
        "cohort",
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that a cohort is consistent with
#' age and time columns, if present
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_cohort_consistent <- function(df, nm_df) {
  cohort <- df[["cohort"]]
  has_age <- "age" %in% names(df)
  has_time <- "time" %in% names(df)
  if (has_age && has_time) {
    age <- df[["age"]]
    time <- df[["time"]]
    is_consistent <- (time - cohort - age) %in% 0:1
    i_inconsistent <- match(FALSE, is_consistent, nomatch = 0L)
    if (i_inconsistent > 0L) {
      stop(
        gettextf(
          "'%s' [%d], '%s' [%d], and '%s' [%d] in row %d of data frame '%s' are inconsistent",
          "age",
          age[[i_inconsistent]],
          "time",
          time[[i_inconsistent]],
          "cohort",
          cohort[[i_inconsistent]],
          i_inconsistent,
          nm_df
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check 'collapse' argument is NULL or a vector
#' formed from "age", "sex", "cohort"
#'
#' @param collapse A character vector or NULL
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_collapse <- function(collapse, can_collapse_time) {
  choices <- c("age", "sex", "cohort")
  if (can_collapse_time) {
    choices <- c(choices, "time")
  }
  if (!is.null(collapse)) {
    checkmate::assert_character(collapse,
      min.chars = 1L,
      unique = TRUE
    )
    is_valid <- collapse %in% choices
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L) {
      stop(
        gettextf(
          "'%s' is not a valid value for '%s'",
          collapse[[i_invalid]],
          "collapse"
        ),
        call. = FALSE
      )
    }
    if (all(c("age", "cohort", "time") %in% collapse)) {
      stop(
        gettextf(
          paste(
            "cannot simultaneously collapse \"%s\", \"%s\",",
            "and \"%s\" : need to retain at least one of the three"
          ),
          "age",
          "cohort",
          "time"
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that data models have the right class
#' and are mutually compatible
#'
#' @param datamods
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_datamods <- function(datamods) {
  ## list where elements have class "dpmaccount_datamod"
  checkmate::assert_list(datamods,
    types = "dpmaccount_datamod"
  )
  ## no duplicate values for 'nm_data'
  nms_data <- vapply(datamods, get_nm_data, "")
  is_dup <- duplicated(nms_data)
  i_dup <- match(TRUE, is_dup, nomatch = 0L)
  if (i_dup > 0L) {
    stop(
      gettextf(
        "two data models with the same value for '%s' [\"%s\"]",
        "nm_data",
        nms_data[[i_dup]]
      ),
      call. = FALSE
    )
  }
  ## has exactly one data model for births and one for deaths
  nms_series <- vapply(datamods, get_nm_series, "")
  for (nm in c("births", "deaths")) {
    n_found <- sum(nms_series == nm)
    if (n_found == 0L) {
      stop(
        gettextf(
          "no data model for series \"%s\"",
          nm
        ),
        call. = FALSE
      )
    }
    if (n_found > 1L) {
      stop(
        gettextf(
          "two data models for series \"%s\"",
          nm
        ),
        call. = FALSE
      )
    }
  }
  ## births and deaths have same age and sex categories
  i_bth <- match("births", nms_series)
  i_dth <- match("deaths", nms_series)
  classif_vars_bth <- get_classif_vars(datamods[[i_bth]])
  classif_vars_dth <- get_classif_vars(datamods[[i_dth]])
  for (vname in c("sex", "time")) {
    var_bth <- classif_vars_bth[[vname]]
    var_dth <- classif_vars_dth[[vname]]
    if (!setequal(var_bth, var_dth)) {
      stop(
        gettextf(
          paste(
            "data models for births and deaths have inconsistent",
            "categories for variable '%s'"
          ),
          vname
        ),
        call. = FALSE
      )
    }
  }
  ## return
  invisible(TRUE)
}



#' Check a degrees-of-freedom argument
#'
#' @param df Degrees of freedom.
#' A positive finite double.
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_df <- function(df) {
  if (!is.numeric(df)) {
    stop(gettextf(
      "'%s' has class \"%s\"",
      "df",
      class(df)
    ))
  }
  if (!identical(length(df), 1L)) {
    stop(gettextf(
      "'%s' has length %s",
      "df",
      length(df)
    ))
  }
  if (is.na(df)) {
    stop(gettextf(
      "'%s' is NA",
      "df"
    ))
  }
  if (is.infinite(df)) {
    stop(gettextf(
      "'%s' is infinite",
      "df"
    ))
  }
  if (df <= 0) {
    stop(gettextf(
      "'%s' is non-positive",
      "df"
    ))
  }
  invisible(TRUE)
}







## HAS_TESTS
#' Check whether data frame has required classification variables
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#' @param is_popn Whether data frame describes population.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_df_classif_nms_complete <- function(df, nm_df, is_popn) {
  nms_df <- names(df)
  nms_classif_vars <- c("age", "sex", "time")
  if (!is_popn) {
    nms_classif_vars <- c(nms_classif_vars, "cohort")
  }
  is_nm_found <- nms_classif_vars %in% nms_df
  i_nm_absent <- match(FALSE, is_nm_found, nomatch = 0L)
  if (i_nm_absent > 0L) {
    stop(
      gettextf(
        "data frame '%s' does not have \"%s\" variable",
        nm_df,
        nms_classif_vars[[i_nm_absent]]
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check a data frame to be used as the 'data' argument
#' in a data model
#'
#' The data frame must have all classifying
#' variables (so that the data has maximum detail,
#' and values are not shared across cohorts/sexes)
#' but can be missing combinations of the variables.
#' The 'count' variable can also be NA.
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#' @param is_popn Whether the data frame is
#' describing population counts.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_df_data <- function(df, nm_df, is_popn) {
  ## data frame
  val <- checkmate::check_data_frame(df)
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with '%s' : %s",
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  ## all names
  check_df_nms(
    df = df,
    nm_df = nm_df,
    nm_measure_var = "count",
    is_popn = is_popn
  )
  check_df_classif_nms_complete(
    df = df,
    nm_df = nm_df,
    is_popn = is_popn
  )
  check_df_measure_nm_complete(
    df = df,
    nm_df = nm_df,
    nm_measure_var = "count"
  )
  ## age variable
  check_age(
    df = df,
    nm_df = nm_df
  )
  ## sex variable
  check_sex(
    df = df,
    nm_df = nm_df
  )
  ## cohort variable
  if (!is_popn) {
    check_cohort(
      df = df,
      nm_df = nm_df
    )
    check_cohort_consistent(
      df = df,
      nm_df = nm_df
    )
  }
  ## time variable
  check_time(
    df = df,
    nm_df = nm_df
  )
  ## classification variables have no duplicates
  check_classif_no_dup(
    df = df,
    nm_df = nm_df
  )
  ## measure variable
  check_measure_var(
    df = df,
    nm_df = nm_df,
    nm_measure_var = "count"
  )
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check a data frame holding constants to be
#' used in a data model
#'
#' The data frame does not need to have all
#' classifying variables, but must have all
#' combinations of the variables that are present.
#' The data frame is allowed to have combinations
#' not found in the 'data' argument.
#' (These extra combinations are silently dropped.)
#' The measurement variable can only be NA if the
#' corresponding count from 'data' is NA.
#'
#' @param df A data frame.
#' @param nm_df The name of the data frame.
#' @param nm_measure_var Name of the measurement variable.
#' @param is_popn Whether the data is
#' population counts.
#' @param data Data frame constaining dataset
#' being modelled.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_df_dataconst <- function(df,
                               nm_df,
                               nm_measure_var,
                               is_popn,
                               data) {
  ## data frame
  val <- checkmate::check_data_frame(df)
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with '%s' : %s",
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  nms_df <- names(df)
  ## all names
  check_df_nms(
    df = df,
    nm_df = nm_df,
    nm_measure_var = nm_measure_var,
    is_popn = is_popn
  )
  check_df_measure_nm_complete(
    df = df,
    nm_df = nm_df,
    nm_measure_var = nm_measure_var
  )
  ## classification variables have no duplicates
  check_classif_no_dup(
    df = df,
    nm_df = nm_df
  )
  ## measure variable
  check_measure_var(
    df = df,
    nm_df = nm_df,
    nm_measure_var = nm_measure_var
  )
  for (nm in c("count", ".is_na")) {
    if (nm %in% nms_df) {
      stop(
        gettextf(
          "'%s' has a variable called '%s'",
          nm_df,
          nm
        ),
        call. = FALSE
      )
    }
  }
  ## prepare 'combined' df to use in subsequent
  ## two checks
  df$.is_na <- is.na(df[[nm_measure_var]])
  nms_classif_vars <- setdiff(nms_df, nm_measure_var)
  data_ag <- data
  data_ag$count <- !is.na(data_ag$count)
  data_ag <- stats::aggregate(data_ag["count"],
    by = data_ag[nms_classif_vars],
    FUN = sum
  )
  combined <- merge(data_ag, df, by = nms_classif_vars, all.x = TRUE)
  ## check 'df' has all levels found in 'data'
  is_missing <- is.na(combined[[".is_na"]])
  i_missing <- match(TRUE, is_missing, nomatch = 0L)
  if (i_missing > 0L) {
    vals <- combined[i_missing, nms_classif_vars]
    vals <- sapply(vals, as.character)
    vals <- sprintf("'%s'", vals)
    vals <- paste(vals, collapse = ", ")
    stop(
      gettextf(
        "'%s' has row with values %s, but '%s' does not",
        "data",
        vals,
        nm_df
      ),
      call. = FALSE
    )
  }
  ## check that measure var for 'df' is missing
  ## only if 'count' var for data is
  is_na <- combined[[".is_na"]] & (combined[["count"]] > 0L)
  i_na <- match(TRUE, is_na, nomatch = 0L)
  if (i_na > 0L) {
    vals <- combined[i_na, nms_classif_vars]
    vals <- sapply(vals, as.character)
    vals <- sprintf("'%s'", vals)
    vals <- paste(vals, collapse = ", ")
    stop(
      gettextf(
        paste(
          "'%s' variable in '%s' is %s but '%s' variable in '%s' is %s",
          "for classification variables %s"
        ),
        nm_measure_var,
        nm_df,
        "NA",
        "count",
        "data",
        "non-NA",
        vals
      ),
      call. = FALSE
    )
  }
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check whether data frame has measurement variable
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#' @param nm_measure_var Name of measurement variable
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_df_measure_nm_complete <- function(df, nm_df, nm_measure_var) {
  nms_df <- names(df)
  if (!(nm_measure_var %in% nms_df)) {
    stop(
      gettextf(
        "data frame '%s' does not have \"%s\" variable",
        nm_df,
        nm_measure_var
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check whether all variable names in a data frame are valid
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#' @param nm_measure_var Name of measurement variable
#' @param is_popn Whether data frame describes population.#'
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_df_nms <- function(df, nm_df, nm_measure_var, is_popn) {
  nms_classif_vars <- c("age", "sex", "time")
  if (!is_popn) {
    nms_classif_vars <- c(nms_classif_vars, "cohort")
  }
  nms_df <- names(df)
  ## no duplicates
  is_dup <- duplicated(nms_df)
  i_dup <- match(TRUE, is_dup, nomatch = 0L)
  if (i_dup > 0L) {
    stop(
      gettextf(
        "data frame '%s' has two variables called \"%s\"",
        nm_df,
        nms_df[[i_dup]]
      ),
      call. = FALSE
    )
  }
  ## all valid
  nms_valid <- c(nms_classif_vars, nm_measure_var)
  is_nm_valid <- nms_df %in% nms_valid
  i_nm_invalid <- match(FALSE, is_nm_valid, nomatch = 0L)
  if (i_nm_invalid > 0L) {
    stop(
      gettextf(
        "data frame '%s' has variable called \"%s\"",
        nm_df,
        nms_df[[i_nm_invalid]]
      ),
      call. = FALSE
    )
  }
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check a 'mean' or 'disp' data frame in a
#' system model
#'
#' The data frame does not need to have all
#' classifying variables, but must have all
#' combinations of the variables that are present.
#' The measurement variable cannot be NA.
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#' @param is_births Whether the data frame is
#' describing birth rates or counts.
#' @param nm_measure_var Name of measurement
#' variable.
#'
#' @noRd
check_df_sysmod <- function(df, nm_df, is_births, nm_measure_var) {
  ## data frame
  val <- checkmate::check_data_frame(df, any.missing = FALSE)
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with '%s' : %s",
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  nms_df <- names(df)
  ## all names
  check_df_nms(
    df = df,
    nm_df = nm_df,
    nm_measure_var = nm_measure_var,
    is_popn = FALSE
  )
  check_df_measure_nm_complete(
    df = df,
    nm_df = nm_df,
    nm_measure_var = nm_measure_var
  )
  ## age variable
  has_age <- "age" %in% nms_df
  if (has_age) {
    check_age(
      df = df,
      nm_df = nm_df
    )
    check_age_complete(
      df = df,
      nm_df = nm_df,
      is_births = is_births
    )
  }
  ## sex variable
  has_sex <- "sex" %in% nms_df
  if (has_sex) {
    check_sex(
      df = df,
      nm_df = nm_df
    )
    check_sex_complete(
      df = df,
      nm_df = nm_df
    )
  }
  ## cohort variable
  has_cohort <- "cohort" %in% nms_df
  if (has_cohort) {
    check_cohort(
      df = df,
      nm_df = nm_df
    )
    check_cohort_consistent(
      df = df,
      nm_df = nm_df
    )
  }
  ## time variable
  has_time <- "time" %in% nms_df
  if (has_time) {
    check_time(
      df = df,
      nm_df = nm_df
    )
    check_time_complete(
      df = df,
      nm_df = nm_df
    )
  }
  ## classification variables have no duplicates
  check_classif_no_dup(
    df = df,
    nm_df = nm_df
  )
  ## classification variables complete
  check_classif_complete(
    df = df,
    nm_df = nm_df
  )
  ## measure variable
  check_measure_var(
    df = df,
    nm_df = nm_df,
    nm_measure_var = nm_measure_var
  )
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'nm_series' is "births" or "deaths"
#'
#' @param nm_series
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_bth_dth <- function(nm_series) {
  if (!(nm_series %in% c("births", "deaths"))) {
    stop(
      gettextf(
        "'%s' is \"%s\"",
        "nm_series",
        nm_series
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'nm_series' is "births", "deaths",
#' "ins", or "outs"
#'
#' @param nm_series
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_events <- function(nm_series) {
  if (!(nm_series %in% c(
    "births",
    "deaths",
    "ins",
    "outs"
  ))) {
    stop(
      gettextf(
        "'%s' is \"%s\"",
        "nm_series",
        nm_series
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'nm_series' is not "births" or "deaths"
#'
#' @param nm_series
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_not_bth_dth <- function(nm_series) {
  if (nm_series %in% c("births", "deaths")) {
    stop(
      gettextf(
        "'%s' is \"%s\"",
        "nm_series",
        nm_series
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that a measurement variable is valid
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#' @param nm_measure_var The name of the measurement
#' variable.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_measure_var <- function(df, nm_df, nm_measure_var) {
  measure_var <- df[[nm_measure_var]]
  val <- checkmate::check_numeric(measure_var,
    finite = TRUE
  )
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with variable '%s' in data frame '%s' : %s",
        nm_measure_var,
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that system and data models are consistent with each other
#'
#' Check that "deaths" models have the same categores for
#' classification variables - though the system model for
#' deaths does not necessarily have all classification variables.
#'
#' Also check that the system model for births covers
#' all the ages included in the births data.
#'
#' @param sysmods A list of objects of class
#' "dpmaccount_sysmod"
#' @param datamods A list of objects of class
#' "dpmaccount_datamod"
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_mods <- function(sysmods, datamods) {
  nms_series_sys <- vapply(sysmods, get_nm_series, "")
  nms_series_data <- vapply(datamods, get_nm_series, "")
  ## deaths
  i_dth_sys <- match("deaths", nms_series_sys)
  i_dth_data <- match("deaths", nms_series_data)
  classif_vars_sys <- get_classif_vars(sysmods[[i_dth_sys]])
  classif_vars_data <- get_classif_vars(datamods[[i_dth_data]])
  for (vname in c("age", "sex", "cohort", "time")) {
    var_sys <- classif_vars_sys[[vname]]
    if (!is.null(var_sys)) {
      var_data <- classif_vars_data[[vname]]
      if (!setequal(var_sys, var_data)) {
        stop(
          gettextf(
            paste(
              "variable '%s' in system model for deaths has",
              "different categories from variable '%s'",
              "in data model for deaths"
            ),
            vname,
            vname
          ),
          call. = FALSE
        )
      }
    }
  }
  ## births
  i_bth_sys <- match("births", nms_series_sys)
  i_bth_data <- match("births", nms_series_data)
  classif_vars_sys <- get_classif_vars(sysmods[[i_bth_sys]])
  classif_vars_data <- get_classif_vars(datamods[[i_bth_data]])
  if ("age" %in% names(classif_vars_sys)) {
    levels_age_sys <- classif_vars_sys[["age"]]
    levels_age_data <- classif_vars_data[["age"]]
    is_in_sys <- levels_age_data %in% levels_age_sys
    i_not_in_sys <- match(FALSE, is_in_sys, nomatch = 0L)
    if (i_not_in_sys > 0L) {
      age_missing <- levels_age_data[[i_not_in_sys]]
      stop(
        gettextf(
          paste(
            "data model for births has data for age %d,",
            "but system model for births does not have rate for age %d"
          ),
          age_missing,
          age_missing
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


#' Check that the name of a dataset does not clash with
#' the name of a demographic series
#'
#' @param nm_data Name of the dataset (a string)
#'
#' @returns TRUE, invisbly
#'
#' @noRd
check_nm_data_clash <- function(nm_data) {
  nms_series <- c(
    "population",
    "births",
    "deaths",
    "ins",
    "outs"
  )
  if (nm_data %in% nms_series) {
    txt <- paste(
      "dataset has same name [\"%s\"] as a demographic series :",
      "demographic series are %s"
    )
    stop(
      gettextf(
        txt,
        nm_data,
        paste(sprintf("\"%s\"", nms_series), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check a scale term
#'
#' Check that 'x' is a positive or non-negative
#' finite scalar.
#'
#' @param x A positive or non-negative number.
#' @param x_arg Name for `x` to be
#' used in error messages.
#' @param zero_ok Whether 'x' can be zero.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_scale <- function(x, x_arg, zero_ok) {
  if (!is.numeric(x)) {
    stop(
      gettextf(
        "'%s' has class \"%s\"",
        x_arg,
        class(x)
      ),
      call. = FALSE
    )
  }
  if (length(x) != 1L) {
    stop(
      gettextf(
        "'%s' has length %d",
        x_arg,
        length(x)
      ),
      call. = FALSE
    )
  }
  if (is.na(x)) {
    stop(
      gettextf(
        "'%s' is NA",
        x_arg
      ),
      call. = FALSE
    )
  }
  if (is.infinite(x)) {
    stop(
      gettextf(
        "'%s' is infinite",
        x_arg
      ),
      call. = FALSE
    )
  }
  if (zero_ok) {
    if (x < 0) {
      stop(
        gettextf(
          "'%s' is negative'",
          x_arg
        ),
        call. = FALSE
      )
    }
  } else {
    if (x <= 0) {
      stop(
        gettextf(
          "'%s' is non-positive",
          x_arg
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that a sex variable is valid
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_sex <- function(df, nm_df) {
  sex <- df[["sex"]]
  if (is.factor(sex)) {
    sex <- as.character(sex)
  }
  val <- checkmate::check_character(sex,
    min.chars = 1L,
    any.missing = FALSE
  )
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with variable '%s' in data frame '%s' : %s",
        "sex",
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'sex' variable has Female category
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_sex_complete <- function(df, nm_df) {
  sex <- df[["sex"]]
  if (!("Female" %in% sex)) {
    stop(
      gettextf(
        "'%s' variable in data frame '%s' does not have category \"%s\"",
        "sex",
        nm_df,
        "Female"
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that system models are jointly valid
#'
#' Check that 'sysmod' contains the correct
#' system models, and that these models have
#' consistent classification variables.
#'
#' @param sysmods A list of four system models
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_sysmods <- function(sysmods) {
  ## list with 4 elements, each of which has class "dpmaccount_sysmod"
  checkmate::assert_list(sysmods,
    types = "dpmaccount_sysmod",
    len = 4L
  )
  ## values for 'nm_series' unique
  nms_series <- vapply(sysmods, get_nm_series, "")
  is_dup <- duplicated(nms_series)
  i_dup <- match(TRUE, is_dup, nomatch = 0L)
  if (i_dup > 0L) {
    stop(
      gettextf(
        "two system models have the same value for '%s' [\"%s\"]",
        "nm_series",
        nms_series[[i_dup]]
      ),
      call. = FALSE
    )
  }
  ## same classification variables (with possible
  ## exception of age for births)
  l_classif_vars <- lapply(sysmods, get_classif_vars)
  is_births <- nms_series == "births"
  has_age <- vapply(l_classif_vars, function(x) "age" %in% names(x), TRUE)
  ages <- lapply(l_classif_vars[!is_births & has_age], function(x) x$age)
  if (!elements_setequal(ages)) {
    stop(
      gettextf(
        "system models have inconsistent categories for variable '%s'",
        "age"
      ),
      call. = FALSE
    )
  }
  for (vname in c("sex", "cohort", "time")) {
    has_var <- vapply(l_classif_vars, function(x) vname %in% names(x), TRUE)
    if (any(has_var)) {
      vars <- lapply(l_classif_vars[has_var], function(x) x[[vname]])
      if (!elements_setequal(vars)) {
        stop(
          gettextf(
            "system models have inconsistent categories for variable '%s'",
            vname
          ),
          call. = FALSE
        )
      }
    }
  }
  ## return
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'time' variable is valid
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_time <- function(df, nm_df) {
  time <- df[["time"]]
  val <- checkmate::check_integerish(time,
    any.missing = FALSE
  )
  if (!isTRUE(val)) {
    stop(
      gettextf(
        "problem with variable '%s' in data frame '%s' : %s",
        "time",
        nm_df,
        val
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'time' variable has complete intermediate levels
#'
#' @param df A data frame
#' @param nm_df The name of the data frame.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_time_complete <- function(df, nm_df) {
  time <- df[["time"]]
  complete_time <- seq.int(from = min(time), to = max(time))
  is_found_time <- complete_time %in% time
  i_not_found_time <- match(FALSE, is_found_time, nomatch = 0L)
  if (i_not_found_time > 0L) {
    stop(
      gettextf(
        "'%s' variable in data frame '%s' has gaps : missing value '%d'",
        "time",
        nm_df,
        complete_time[[i_not_found_time]]
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
