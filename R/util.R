## HAS_TESTS
#' Check that all elements of a list are set-equal
#'
#' @param l A list
#'
#' @returns TRUE or FALSE
#'
#' @noRd
elements_setequal <- function(l) {
  n <- length(l)
  if (n < 2L) {
    return(TRUE)
  }
  el_1 <- l[[1L]]
  for (i in seq.int(from = 2L, to = n)) {
    el_i <- l[[i]]
    if (!setequal(el_1, el_i)) {
      return(FALSE)
    }
  }
  TRUE
}


## HAS_TESTS
#' Do a full join on two datasets, based on time
#'
#' Do a full join (ie a join keeping rows from x and y)
#' using "time" as 'by' column.
#'
#' @param x,y Data frames
#'
#' @returns A data frame.
#'
#' @noRd
full_join_time <- function(x, y) {
  merge(x, y, all.x = TRUE, all.y = TRUE, by = "time")
}


## HAS_TESTS
#' Do a full join on two datasets, based on age and time
#'
#' Do a full join (ie a join keeping rows from x and y)
#' using "time", and "age" as 'by' columns.
#'
#' @param x,y Data frames
#'
#' @returns A data frame.
#'
#' @noRd
full_join_timeage <- function(x, y) {
  merge(x, y, all.x = TRUE, all.y = TRUE, by = c("time", "age"))
}


## HAS_TESTS
#' Convert matrix to list of columns
#'
#' Split matrix column by column
#'
#' @param m A matrix
#'
#' @returns A list of length ncol(m),
#' each element of which is a column
#' of length nrow(m).
#'
#' @noRd
matrix_to_list_cols <- function(m) {
  ans <- apply(m, MARGIN = 2L, FUN = unname, simplify = FALSE)
  ans <- unname(ans)
  ans
}


## HAS_TESTS
#' Nest data within groups, formatting data as
#' data frames
#'
#' Columns named in 'nms_data' are turned into
#' miniature data frames, indexed by
#' the columns in 'nms_group'. The new column
#' containing the miniature data frames is called
#' 'nm_value'.
#'
#' Simplified version of tidyr::nest. See help document for tidyr::nest for
#' more information
#'
#' @param df A data frame
#' @param nms_data Names of variables in 'df'.
#' @param nms_group Names of variables in 'df'.
#' @param nm_value Name for column holding
#' mini data frames.
#'
#' @returns A data frame
#'
#' @noRd
nest_to_df <- function(df, nms_data, nms_group, nm_value) {
  data <- df[nms_data]
  group <- df[nms_group]
  l <- split(data, group, lex.order = TRUE)
  unrowname <- function(x) {
    rownames(x) <- NULL
    x
  }
  l <- lapply(l, unrowname)
  split_to_df(
    l = l,
    nms_group = nms_group,
    nm_value = nm_value
  )
}


## HAS_TESTS
#' Convert the results from function 'base::split' into
#' a data frame with a list column
#'
#' Includes converting 'age', 'cohort', and 'time'
#' columns to integer.
#'
#' @param l Results from a call to function 'split'.
#' @param nms_groups The names of the variables used
#' as the 'f' argument in 'split'
#' @param nm_value The name of the list column.
#'
#' @returns A tibble
#'
#' @noRd
split_to_df <- function(l, nms_group, nm_value) {
  ans <- strsplit(names(l), split = ".", fixed = TRUE)
  ans <- do.call(rbind, ans)
  ans <- data.frame(ans)
  ans <- tibble::tibble(ans)
  colnames(ans) <- nms_group
  is_int <- nms_group %in% c("age", "cohort", "time")
  ans[is_int] <- lapply(ans[is_int], as.integer)
  ans[[nm_value]] <- unname(l)
  ans
}


## HAS_TESTS
#' Summarise data from a list of codatamods
#'
#' @param mod A list of objects of class "dpmaccount_codatamod"
#'
#' @returns A tibble, or NULL if 'mods' has length 0
#'
#' @noRd
summarise_codatamods <- function(mods) {
  n <- length(mods)
  if (n == 0L) {
    return(NULL)
  }
  nms <- names(mods)
  ans <- .mapply(get_data_cols,
    dots = list(x = mods, nm = nms),
    MoreArgs = list()
  )
  ans <- Reduce(full_join_timeage, ans)
  ans
}


## HAS_TESTS
#' Construct means of ins, outs, and population by exponentiating the
#' output from TMB
#'
#' @param mean Named numeric vector produced by TMB
#' @param prior_stk_init Named list
#'
#' @returns A tibble
#'
#' @noRd
summarise_estimated_means <- function(mean, prior_stk_init, dths) {
  nms <- names(mean)
  is_stk_init_estimated <- "log_val_stk_init" %in% nms
  if (is_stk_init_estimated) {
    stk_init <- exp(mean[nms == "log_val_stk_init"])
  } else {
    stk_init <- prior_stk_init$mean
  }
  mean_ins <- exp(mean[nms == "log_val_ins"])
  mean_outs <- exp(mean[nms == "log_val_outs"])
  mean_stk_end <- stk_init + cumsum(mean_ins) - cumsum(mean_outs) - cumsum(dths)
  data.frame(
    mean_stk_end = unname(mean_stk_end),
    mean_ins = unname(mean_ins),
    mean_outs = unname(mean_outs)
  )
}


## HAS_TESTS
#' Summarise data from a list of system models
#'
#' @param mod A list of data frames.
#'
#' @returns A tibble, or NULL if 'mods' has length 0
#'
#' @noRd
summarise_sysmods <- function(mods) {
  get_cols <- function(x, nm) {
    ans <- x[c("time", "age", "mean")]
    names(ans)[[3L]] <- nm
    ans
  }
  nms <- names(mods)
  ans <- .mapply(get_cols,
    dots = list(x = mods, nm = nms),
    MoreArgs = list()
  )
  ans <- Reduce(full_join_timeage, ans)
  ans
}


## HAS_TESTS
#' Randomly Generate a list of Integers suitable for
#' Using as Random Seeds
#'
#' @param seed_in the original seed to set the seed for all the subsequent seeds
#'
#' @returns An list containing a seed for each function that requires randomness
#'
#' @noRd
make_seed_account <- function(seed_in = NULL) {
  if (!is.null(seed_in)) {
    set.seed(seed_in)
  }
  samp_vec <- sample.int(n = .Machine$integer.max, size = 9)
  seed_list <- list(
    prior_rate_seed = samp_vec[1],
    draw_counts_seed = samp_vec[2],
    draw_par_datamods_seed = samp_vec[3],
    draw_rates_ins_seed = samp_vec[4],
    draw_rates_outs_seed = samp_vec[5],
    draw_rates_dth_seed = samp_vec[6],
    draw_rates_bth_seed = samp_vec[7],
    post_pred_pop_seed = samp_vec[8],
    post_pred_events_seed = samp_vec[9]
  )
  return(seed_list)
}


#' Convert the seed_list in an old (pre-v0.6.3) dpmaccount_results object
#'
#' Function to convert the seed_list in an old (pre-v0.6.3) dpmaccount_results
#' object to work with v0.6.3-onwards components methods (augment_population,
#' augment_events, augment_rates)
#'
#' @param res_obj A pre-v0.6.3 dpmaccount_results object
#' @param seed_in Integer seed to set the seed for all the subsequent seeds
#'
#' @returns An dpmaccount_results object
#'
#' @noRd
convert_old_dpmaccount_results <- function(res_obj, seed_in = NULL) {
  if (!is.null(seed_in)) {
    set.seed(seed_in)
  }
  res_obj$seed_list <- lapply(1:length(res_obj$cohort), function(x) make_seed_account())
  return(res_obj)
}
