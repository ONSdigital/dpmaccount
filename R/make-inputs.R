## HAS_TESTS
#' Add initial values to classification variables
#'
#' Convert classification variables for events
#' to classification variables for data on
#' stocks, by adding initial values for cohorts
#' that already exist. The function does
#' not create initial values for new cohorts,
#' since these consist of births, which are
#' not included in data models.
#'
#' @param classif_vars Data frame with classification
#' variables for events.
#'
#' @returns A data frame with extra rows added giving
#' classifications variables for stock data (other
#' than births.)
#'
#' @noRd
add_init_to_classif_vars <- function(classif_vars) {
  min_time <- min(classif_vars$time)
  is_min_time <- with(classif_vars, time == min(time))
  is_upper <- with(classif_vars, cohort == time - age - 1L)
  initial <- classif_vars[is_min_time & is_upper, ]
  initial$time <- initial$time - 1L
  ans <- rbind(initial, classif_vars)
  ans <- ans[c("cohort", "sex", "time", "age")]
  ord <- with(ans, order(cohort, sex, time, age))
  ans <- ans[ord, ]
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Make classification variables from system models
#'
#' Use the classification variables from the system
#' model for deaths as the classification variables
#' for the whole account.
#'
#' @param sysmod A list of object of class
#' 'dpmaccount_datamod'.
#'
#' @returns A data frame.
#'
#' @noRd
make_classif_vars <- function(datamods) {
  nms_series <- vapply(datamods, get_nm_series, "")
  i_dth <- match("deaths", nms_series)
  datamod_dth <- datamods[[i_dth]]
  data_dth <- datamod_dth$data
  ans <- data_dth[c("cohort", "sex", "time", "age")]
  ord <- with(ans, order(cohort, sex, time, age))
  ans <- ans[ord, ]
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Make counts of births and deaths by cohort and sex
#'
#' The counts are obtained from the data models
#' for births and deaths, which are assumed
#' to be error-free.
#'
#' Births are tabulated by the cohort, age, and sex of
#' the parent, not the child
#'
#' @param datamods A list of data models
#'
#' @returns A data frame with columns 'cohort', 'sex',
#' and 'count'.
#'
#' @noRd
make_cobthdth_df <- function(datamods) {
  nms_series <- vapply(datamods, get_nm_series, "")
  nms_data <- vapply(datamods, get_nm_data, "")
  i_births <- match("births", nms_series)
  i_deaths <- match("deaths", nms_series)
  ## births: assume single dataset with perfect data,
  ## including only the reproductive ages
  df_bth <- datamods[[i_births]]$data
  labels_sex <- unique(df_bth$sex)
  n_sex <- length(labels_sex)
  df_bth <- nest_to_list(
    df = df_bth,
    nm_data = "count",
    nm_id = "sex",
    nms_group = c("age", "cohort", "time"),
    nm_value = "val_bth"
  )
  df_bth <- df_bth[vapply(df_bth$val_bth, length, 0L) > 0L, , drop = FALSE]
  df_bth$sex <- "Female"
  df_bth$has_bth <- TRUE ## 'has_bth' = Female and in repr ages
  ## deaths: assume single dataset with perfect data covering entire popn
  df_dth <- datamods[[i_deaths]]$data
  names(df_dth)[match("count", names(df_dth))] <- "val_dth"
  ## combine and create cohort-sex-level "counts" lists
  df <- merge(df_bth, df_dth, all.y = TRUE)
  df$has_bth[is.na(df$has_bth)] <- FALSE
  missing <- rep(NA_real_, times = n_sex)
  names(missing) <- labels_sex
  df$val_bth[!df$has_bth] <- rep(list(missing), times = sum(!df$has_bth))
  df$has_bth <- as.integer(df$has_bth)
  df$nm_data_bth <- nms_data[[i_births]]
  df$nm_data_dth <- nms_data[[i_deaths]]
  ord <- with(df, order(cohort, sex, time, age))
  df <- df[ord, ]
  nest_to_df(
    df = df,
    nms_data = c(
      "has_bth",
      "val_bth",
      "val_dth",
      "nm_data_bth",
      "nm_data_dth",
      "time",
      "age"
    ),
    nms_group = c("cohort", "sex"),
    nm_value = "count_bthdth"
  )
}


## HAS_TESTS
#' Combine data frames containing 'codatamod' objects for
#' individual data models into a single data frame
#'
#' @param A list of data frames constructed using
#' function 'make_codatamod_df'.
#'
#' @returns A data frame with columns 'cohort', 'sex',
#' 'datamods_stk', 'datamods_ins', and 'datamods_outs'.
#'
#' @noRd
make_codatamods_df <- function(datamods, classif_vars) {
  ## remove data models for births and deaths
  nms_series <- vapply(datamods, get_nm_series, "")
  is_bth_dth <- nms_series %in% c("births", "deaths")
  if (any(!is_bth_dth)) {
    datamods <- datamods[!is_bth_dth]
    ## convert to data frames with codatamod objects
    l <- lapply(datamods, make_codatamod_df, classif_vars = classif_vars)
    ## group together data models for the same demographic series
    df <- do.call(rbind, l)
    df <- nest_to_list(
      df = df,
      nm_data = "datamod",
      nm_id = "nm_data",
      nms_group = c("cohort", "sex", "nm_series"),
      nm_value = "datamods"
    )
    ## put into wide format - do by hand because 'reshape'
    ## doesn't work properly with lists
    df_stk <- df[df$nm_series == "population",
      c("cohort", "sex", "datamods"),
      drop = FALSE
    ]
    df_ins <- df[df$nm_series == "ins",
      c("cohort", "sex", "datamods"),
      drop = FALSE
    ]
    df_outs <- df[df$nm_series == "outs",
      c("cohort", "sex", "datamods"),
      drop = FALSE
    ]
    names(df_stk)[match("datamods", names(df_stk))] <- "datamods_stk"
    names(df_ins)[match("datamods", names(df_ins))] <- "datamods_ins"
    names(df_outs)[match("datamods", names(df_outs))] <- "datamods_outs"
    classif_vars <- unique(df[c("cohort", "sex")])
    left_join <- function(x, y) merge(x, y, all.x = TRUE)
    ans <- Reduce(left_join, list(classif_vars, df_stk, df_ins, df_outs))
    empty_lists <- rep(list(list()), times = nrow(ans))
    if (nrow(df_stk) == 0L) {
      ans$datamods_stk <- empty_lists
    }
    if (nrow(df_ins) == 0L) {
      ans$datamods_ins <- empty_lists
    }
    if (nrow(df_outs) == 0L) {
      ans$datamods_outs <- empty_lists
    }
  } else {
    ans <- unique(classif_vars[c("cohort", "sex")])
    empty_lists <- rep(list(list()), times = nrow(ans))
    ans$datamods_stk <- empty_lists
    ans$datamods_ins <- empty_lists
    ans$datamods_outs <- empty_lists
  }
  ## return
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Make data frame holding initial values
#'
#' Make data frame with variables 'cohort',
#' 'sex', and 'prior_stk_init'. Variable 'prior_stk_init'
#' is a list, each element of which is a list
#' with elements 'mean' and 'sd'.
#'
#' For cohorts existing at the start of the
#' estimation period, the initial value is
#' a population estimate; for cohorts born
#' during the estimation period, the initial
#' value is births.
#'
#' @param datamods A list of objects of class
#' 'dpmaccount_datamod'. Only values for
#' deaths and population are used.
#' @param classif_vars Data frame with classification
#' variables.
#'
#' @returns A data frame
#'
#' @noRd
make_costkinit_df <- function(datamods, classif_vars) {
  nms_series <- vapply(datamods, get_nm_series, "")
  is_popn <- nms_series == "population"
  i_bth <- match("births", nms_series)
  min_time <- min(classif_vars$time)
  cohort_sex <- unique(classif_vars[c("cohort", "sex")])
  ## cohorts born during estimation period
  ans_bth <- datamods[[i_bth]]$data ## assume single dataset with perfect data
  ans_bth <- stats::aggregate(
    ans_bth["count"],
    ans_bth[c("sex", "time")],
    sum
  )
  names(ans_bth)[match("time", names(ans_bth))] <- "cohort"
  names(ans_bth)[match("count", names(ans_bth))] <- "mean"
  ans_bth$sd <- 0
  ans_bth$is_new_cohort <- TRUE
  ## cohorts born before start of estimation period
  if (any(is_popn)) {
    datamods_popn <- datamods[is_popn]
    get_init_popn <- function(mod) {
      data <- mod$data
      data[data$time == min_time - 1L,
        c("cohort", "sex", "count"),
        drop = FALSE
      ]
    }
    dfs_popn <- lapply(datamods_popn, get_init_popn)
    ans_popn <- get_best_value_multi(dfs_popn)
    names(ans_popn)[match("value", names(ans_popn))] <- "mean"
    ans_popn$sd <- Inf
    ans_popn$is_new_cohort <- FALSE
    ans <- rbind(ans_bth, ans_popn)
  } else {
    ans <- ans_bth
  }
  ## combine and return
  ans <- merge(cohort_sex, ans, all.x = TRUE)
  ans$is_new_cohort[is.na(ans$is_new_cohort)] <- FALSE
  is_na <- is.na(ans$mean)
  ans$mean[is_na] <- mean(ans$mean[!is_na])
  ans$sd <- ifelse(is_na, Inf, ans$sd)
  ans$prior_stk_init <- .mapply(list,
    dots = ans[c("mean", "sd")],
    MoreArgs = NULL
  )
  ans <- ans[c("cohort", "sex", "is_new_cohort", "prior_stk_init")]
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Create and merge cohort-level system models
#'
#' @param A list of objects of class 'dpmaccount_sysmod'
#' @param A data frame with a complete set of classification
#' variables for the account
#'
#' @returns A data frame with columns "cohort", "sex",
#' "sysmod_bth", "sysmod_dth", "sysmod_ins", "sysmod_outs"
#'
#' @noRd
make_cosysmods_df <- function(sysmods, classif_vars) {
  ans <- lapply(sysmods,
    make_cosysmod_df,
    classif_vars = classif_vars
  )
  ans <- Reduce(merge, ans)
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Make 'map' argument to MakeADFun
#'
#' Make the 'map' argument to TMB::MakeADFun.
#' The 'map' argument can be used to fix
#' parameters at their initial value. At present
#' we only do this 'log_val_stk_init', ie
#' the log of the initial stock, which we
#' fix when its prior standard deviation is 0.
#' (Currently this happens when the initial
#' stock consists of births.)
#'
#' @param prior_stk_init List specifying prior
#' for initial stock.
#'
#' @returns NULL or a named list.
#'
#' @noRd
make_map <- function(prior_stk_init) {
  sd <- prior_stk_init$sd
  if (sd > 0) {
    NULL
  } else {
    list(log_val_stk_init = factor(NA))
  }
}


## HAS_TESTS
#' Make 'parameters' argument to MakeADFun
#'
#' Make the 'parameters' argument to TMB::MakeADFun.
#' The 'parameters' argument holds the
#' initial values for parameters, and also tells
#' TMB the names and size of parameters.
#' defines the lengths/dimensions of the parameters.
#'
#' We increase the initial value for population
#' to a higher level than is suggested
#' by the data. There appears to be an asymmetry,
#' where TMB can cope with an initial population
#' value that is (much) too high, but cannot
#' cope with one that is too low.
#'
#' @param mean_stk_init Mean in prior for
#' for initial stock.
#' @param sd_stk_init Standard deviation in
#' prior for initial stock
#' @param val_dth Counts of deaths
#' @param means_ins Expected counts from
#' system model for ins
#' @param mean_outs Expected rates from
#' system model for outs
#' @param datamods_stk List of objects of
#' class "dpmaccount_codatamod", containing
#' data and data models for population.
#' @param datamods_ins List of objects of
#' class "dpmaccount_codatamod", containing
#' data on and data models for ins.
#' @param datamods_outs List of objects of
#' class "dpmaccount_codatamod", containing
#' data on and data models for outs.
#'
#' @returns NULL or a named list.
#'
#' @noRd
make_parameters <- function(mean_stk_init,
                            sd_stk_init,
                            val_dth,
                            mean_ins,
                            mean_outs,
                            datamods_stk,
                            datamods_ins,
                            datamods_outs) {
  vals_init <- make_vals_init(
    mean_stk_init = mean_stk_init,
    sd_stk_init = sd_stk_init,
    val_dth = val_dth,
    mean_ins = mean_ins,
    mean_outs = mean_outs
  )
  par_all_stk <- get_par_all(datamods_stk)
  par_all_ins <- get_par_all(datamods_ins)
  par_all_outs <- get_par_all(datamods_outs)
  list(
    log_val_stk_init = vals_init$log_val_stk_init,
    log_val_ins = vals_init$log_val_ins,
    log_val_outs = vals_init$log_val_outs,
    par_all_stk = par_all_stk,
    par_all_ins = par_all_ins,
    par_all_outs = par_all_outs
  )
}


## HAS_TESTS
#' Construct initial values for
#' log_val_ins, and log_val_outs,
#' and possibly val_stk_init
#'
#' All values of val_ins and val_outs,
#' plus derived values for stock,
#' guaranteed to be greater than or equal to 'eps'.
#'
#' If 'val_stk_init' is being treated as uncertain,
#' then it can be different from prior mean.
#' If 'val_stk_init' is being treated as certain
#' prior mean is 0, then set 'val_stk_init' to 'eps'
#' (to avoid convergence problems.)
#'
#' @param mean_stk_init,sd_stk_init
#' Prior for initial stock.
#' @param mean_ins Expected values for val_ins,
#' from system model
#' @param mean_outs Expected values for rate
#' for outs, from system model
#' @param val_dth Death counts
#'
#' @returns A named list
#'
#' @noRd
make_vals_init <- function(mean_stk_init,
                           sd_stk_init,
                           val_dth,
                           mean_ins,
                           mean_outs) {
  max_iter <- 10000
  eps <- 1e-6
  is_stk_init_nonzero <- mean_stk_init > 0
  is_stk_init_estimated <- sd_stk_init > 0
  succeeded <- FALSE
  K <- length(val_dth)
  for (i in seq.int(from = 0L, to = max_iter)) {
    if (is_stk_init_estimated) {
      val_stk_init <- exp(0.01 * i) * mean_stk_init
      val_stk_init <- max(val_stk_init, eps)
    } else {
      if (is_stk_init_nonzero) {
        val_stk_init <- mean_stk_init
      } else {
        val_stk_init <- eps
      }
    }
    val_ins <- exp(0.01 * i) * mean_ins
    val_ins <- pmax(val_ins, eps)
    rate_outs <- exp(-0.01 * i) * mean_outs
    val_outs <- double(length = K)
    stk <- val_stk_init
    for (k in seq_len(K)) {
      stk <- stk + val_ins[k] - val_dth[k]
      if (stk < eps) {
        break
      }
      exposure <- 0.5 * stk ## 0.5 because Lexis triangle
      val_outs[k] <- mean_outs[k] * exposure
      val_outs[k] <- max(val_outs[k], eps)
      stk <- stk - val_outs[k]
      if (stk < eps) {
        break
      }
      if (k == K) {
        succeeded <- TRUE
      }
    }
    if (succeeded) {
      break
    }
  }
  if (!succeeded) {
    stop("Internal error: Unable to generate initial values.",
      call. = FALSE
    )
  }
  list(
    log_val_stk_init = log(val_stk_init),
    log_val_ins = log(val_ins),
    log_val_outs = log(val_outs)
  )
}
