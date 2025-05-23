## 'get_classif_vars' ---------------------------------------------------------

#' Extract classif_vars
#'
#' @param x A data model or system model
#'
#' @returns A data frame
#'
#' @noRd
get_classif_vars <- function(x) {
  UseMethod("get_classif_vars")
}

## HAS_TESTS
#' @export
get_classif_vars.dpmaccount_datamod <- function(x) {
  data <- x$data
  i_count <- match("count", names(data))
  data[-i_count]
}


## 'get_nm_data' --------------------------------------------------------------

#' Extract 'nm_data' component
#'
#' @param x A data model
#'
#' @returns A string.
#'
#' @noRd
get_nm_data <- function(x) {
  UseMethod("get_nm_data")
}

## HAS_TESTS
#' @export
get_nm_data.dpmaccount_datamod <- function(x) x$nm_data


## 'get_nm_series' ------------------------------------------------------------

#' Extract 'nm_series' component
#'
#' @param x A system or data model
#'
#' @returns A string: one of "population", "births",
#' "deaths", "ins", "outs"
#'
#' @noRd
get_nm_series <- function(x) {
  UseMethod("get_nm_series")
}

## HAS_TESTS
#' @export
get_nm_series.dpmaccount_datamod <- function(x) x$nm_series


## 'make_codatamod_df' --------------------------------------------------------

#' Convert a user-supplied data model into cohort
#' data models
#'
#' Turn an 'dpmaccount_datamod' object into a data frame
#' with a list column containing 'dpmaccount_codatamod'
#' objects, plus 'cohort', 'sex', 'nm_series' and 'nm_data'
#' columnns.
#'
#' 'classif_vars' contains *all* combinations of classifying
#' variables, not just those found in the data model
#'
#' @param mod An object of class 'dpmaccount_datamod'
#' @param classif_vars A data frame with complete set
#' of classification variables.
#'
#' @returns A data frame with variables for data model,
#' plus time and age.
#'
#' @noRd
make_codatamod_df <- function(mod, classif_vars) {
  UseMethod("make_codatamod_df")
}


## HAS_TESTS
#' @export
make_codatamod_df.dpmaccount_datamod_norm <- function(mod, classif_vars) {
  data <- mod$data
  ratio <- mod$ratio
  sd <- mod$sd
  scale_ratio <- mod$scale_ratio
  scale_sd <- mod$scale_sd
  nm_series <- mod$nm_series
  nm_data <- mod$nm_data
  is_popn <- nm_series == "population"
  if (is_popn) {
    classif_vars <- add_init_to_classif_vars(classif_vars)
  }
  if (is.numeric(ratio)) {
    tmp <- classif_vars
    tmp$ratio <- ratio
    ratio <- tmp
  }
  left_join <- function(x, y) merge(x, y, all.x = TRUE)
  df <- Reduce(left_join, list(classif_vars, data, ratio, sd))
  df$data <- df$count
  df$is_obs <- as.integer(!is.na(df$data))
  ord <- with(df, order(cohort, sex, time, age))
  df <- df[ord, ]
  df$scale_ratio <- scale_ratio
  df$scale_sd <- scale_sd
  if (is_popn) {
    is_accession <- with(df, age == time - cohort - 1L)
    df$ratio[is_accession] <- NA
    df$sd[is_accession] <- NA
    df$scale_ratio[is_accession] <- NA
    df$scale_sd[is_accession] <- NA
  }
  ans <- nest_to_df(
    df = df,
    nms_data = c(
      "data", "is_obs", "ratio", "sd",
      "scale_ratio", "scale_sd",
      "time", "age"
    ),
    nms_group = c("cohort", "sex"),
    nm_value = "args"
  )
  ans$nm_series <- nm_series
  ans$nm_data <- nm_data
  ans$datamod <- lapply(ans$args, new_codatamod_norm)
  ans <- ans[-match("args", names(ans))]
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' @export
make_codatamod_df.dpmaccount_datamod_t <- function(mod, classif_vars) {
  data <- mod$data
  df_param <- mod$df ## Name clash
  ratio <- mod$ratio
  scale <- mod$scale
  nm_series <- mod$nm_series
  nm_data <- mod$nm_data
  is_popn <- nm_series == "population"
  if (is_popn) {
    classif_vars <- add_init_to_classif_vars(classif_vars)
  }
  if (is.numeric(ratio)) {
    tmp <- classif_vars
    tmp$ratio <- ratio
    ratio <- tmp
  }
  left_join <- function(x, y) merge(x, y, all.x = TRUE)
  df <- left_join(classif_vars, data)
  df$df <- df_param
  df <- Reduce(left_join, list(df, ratio, scale))
  df$data <- df$count
  df$is_obs <- as.integer(!is.na(df$data))
  ord <- with(df, order(cohort, sex, time, age))
  df <- df[ord, ]
  if (is_popn) {
    is_accession <- with(df, age == time - cohort - 1L)
    df$ratio[is_accession] <- NA
    df$scale[is_accession] <- NA
  }
  ans <- nest_to_df(
    df = df,
    nms_data = c("data", "is_obs", "df", "ratio", "scale", "time", "age"),
    nms_group = c("cohort", "sex"),
    nm_value = "args"
  )
  ans$nm_series <- nm_series
  ans$nm_data <- nm_data
  ans$datamod <- lapply(ans$args, new_codatamod_t)
  ans <- ans[-match("args", names(ans))]
  ans <- tibble::tibble(ans)
  ans
}


## 'print' --------------------------------------------------------------------

#' @export
print.dpmaccount_datamod_exact <- function(x, ...) {
  nchar_offset <- 10
  cat("Exact data model : An object of class \"dpmaccount_datamod_exact\"\n\n")
  cat("dataset:", x$nm_data, "\n")
  cat("series:", x$nm_series, "\n")
  cat("data:\n")
  print(summary(x$data))
  invisible(x)
}

#' @export
print.dpmaccount_datamod_norm <- function(x, ...) {
  nchar_offset <- 10
  cat("Normal data model : An object of class \"dpmaccount_datamod_norm\"\n\n")
  cat("dataset:", x$nm_data, "\n")
  cat("series:", x$nm_series, "\n")
  cat("data:\n")
  print(summary(x$data))
  if (is.data.frame(x$ratio)) {
    cat("\nratio:\n")
    print(summary(x$ratio))
  } else {
    cat("\nratio: ", x$ratio, "\n", sep = "")
  }
  cat("\nsd:\n")
  print(summary(x$sd))
  invisible(x)
}


#' @export
print.dpmaccount_datamod_t <- function(x, ...) {
  nchar_offset <- 10
  cat("t-distribution data model : An object of class \"dpmaccount_datamod_t\"\n\n")
  cat("dataset:", x$nm_data, "\n")
  cat("series:", x$nm_series, "\n")
  cat("data:\n")
  print(summary(x$data))
  cat("\ndf: ", x$df, "\n", sep = "")
  if (is.data.frame(x$ratio)) {
    cat("\nratio:\n")
    print(summary(x$ratio))
  } else {
    cat("\nratio: ", x$ratio, "\n", sep = "")
  }
  cat("\nscale:\n")
  print(summary(x$scale))
  invisible(x)
}
